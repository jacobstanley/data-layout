{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Layout.Vector (
      Codec
    , compile

    , StorableVector (..)
    , encodeVectors
    , decodeVector
    ) where

import           Control.Monad (when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Vector.Storable as V
import           Data.Word (Word8, Word32)
import           Foreign.C (CInt (..))
import           Foreign.ForeignPtr ()
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, plusPtr, castPtr)
import           Foreign.Storable (Storable, peek, poke, sizeOf)
import           System.IO.Unsafe (unsafePerformIO)

import           Data.Layout.ForeignPtr (mallocPlainForeignPtrBytes)
import qualified Data.Layout.Language as L
import           Data.Layout.Types (Layout(..), ByteOrder(..))

------------------------------------------------------------------------
-- Vector Encoding Operations

data StorableVector where
    SV :: Storable a => V.Vector a -> StorableVector

encodeVectors :: [(Codec, StorableVector)] -> ByteString
encodeVectors [] = B.empty
encodeVectors xs@((codec, SV vec):_) = unsafePerformIO $ do
    -- allocate memory for the bytestring
    bp <- mallocPlainForeignPtrBytes bstrBytes

    -- encode each vector
    mapM_ (go (DstPtr bp 0)) xs

    -- return the bytestring
    return (PS bp 0 bstrBytes)
  where
    bstrBytes = encodeReps codec vec * encodedSize codec

    go dp (c, SV v) = encodeVector c dp v

encodeVector :: forall a. Storable a => Codec -> DstPtr -> V.Vector a -> IO ()
encodeVector codec dstPtr vec = do
    -- check that the vector type matches the codec
    checkValueSize "encodeVector" codec vec

    -- get a pointer to the vector elements
    let vp = castForeignPtr (fst (V.unsafeToForeignPtr0 vec))

    -- encode the vector
    encode codec n dstPtr (SrcPtr vp 0)
  where
    n = encodeReps codec vec

encodeReps :: Storable a => Codec -> V.Vector a -> Int
encodeReps c v = repetitions "Vector" (vectorSize v) (decodedSize c)

------------------------------------------------------------------------
-- Vector Decoding Operations

decodeVector :: forall a. Storable a => Codec -> ByteString -> V.Vector a
decodeVector codec bstr@(PS bp bpOff _) = unsafePerformIO $ do
    -- check that the vector type matches the codec
    checkValueSize "decodeVector" codec (V.empty :: V.Vector a)

    -- allocate memory for the vector
    vp <- mallocPlainForeignPtrBytes vectorBytes

    -- decode the bytestring in to the vector
    decode codec n (DstPtr vp 0) (SrcPtr bp bpOff)

    -- return the vector
    return (V.unsafeFromForeignPtr0 (castForeignPtr vp) vectorElems)
  where
    n = repetitions "ByteString" (B.length bstr) (encodedSize codec)

    vectorBytes = n * decodedSize codec
    vectorElems = n * valueCount codec

------------------------------------------------------------------------
-- Vector Utils

vectorSize :: forall a. Storable a => V.Vector a -> Int
vectorSize v = V.length v * sizeOf (undefined :: a)

-- | Ensures the value size of the codec matches the vector type.
checkValueSize :: forall a m. (Storable a, Monad m)
               => String -> Codec -> V.Vector a -> m ()
checkValueSize fn codec _ =
    when (codecValueSize /= vectorElemSize) (error errorMsg)
  where
    codecValueSize = valueSize codec
    vectorElemSize = sizeOf (undefined :: a)

    errorMsg = concat
      [ "Data.Layout.Vector.", fn, ": "
      , "Value size mismatch. The value size of a codec ("
      , show codecValueSize, " bytes) did not match the size of "
      , "individual elements (", show vectorElemSize, " bytes) in "
      , "the corresponding vector. This means that the wrong type "
      , "of vector is being used for a given codec." ]

repetitions :: String -> Int -> Int -> Int
repetitions sourceName sourceBytes codecBytes =
    if leftover /= 0 then error msg else n
  where
    (n, leftover) = sourceBytes `quotRem` codecBytes

    msg = concat
      [ "Data.Layout.Vector.encodeReps: "
      , "The source ", sourceName, " is not a multiple of "
      , show codecBytes, " bytes, as required by the Codec. "
      , show sourceBytes, " bytes were provided, which leaves "
      , show leftover, " bytes unused." ]


------------------------------------------------------------------------
-- Ptr Operations

data DstPtr = DstPtr {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int

data SrcPtr = SrcPtr {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int

data Codec = Codec
    { encode      :: Int -> DstPtr -> SrcPtr -> IO ()
    , decode      :: Int -> DstPtr -> SrcPtr -> IO ()
    , encodedSize :: Int
    , decodedSize :: Int
    , valueCount  :: Int
    , valueSize   :: Int
    }

-- | Compiles a data layout in to a codec capable of encoding
-- and decoding data stored in the layout.
compile :: Layout -> Codec
compile layout = Codec
    { encode      = runCodec copyInfo c_encode
    , decode      = runCodec copyInfo c_decode
    , encodedSize = L.size layout
    , decodedSize = L.valueSizeN layout
    , valueCount  = L.valueCount layout
    , valueSize   = L.valueSize1 layout
    }
  where
    copyInfo = buildCopyInfo layout

runCodec :: CopyInfo -> CodecFn -> Int -> DstPtr -> SrcPtr -> IO ()
runCodec info c_codec_fn reps (DstPtr dstFP dstOff) (SrcPtr srcFP srcOff) =
    -- unbox foreign pointers for use
    withForeignPtr dstFP $ \dst0 -> do
    withForeignPtr srcFP $ \src0 -> do

    -- add the offset
    let dst = dst0 `plusPtr` dstOff
        src = src0 `plusPtr` srcOff

    -- get a pointer to the offsets
    V.unsafeWith (ciOffsets info) $ \offsets -> do

    -- decode the data
    err <- c_codec_fn
        dst src
        (fromIntegral reps)
        (ciNumOffsets info)
        offsets
        (ciNumValues info)
        (ciValueSize info)
        (ciSwapBytes info)

    -- check error code
    case err of
      0 -> return ()
      1 -> error ("runCodec: invalid value size: " ++ show (ciValueSize info))
      _ -> error "runCodec: unknown error"


------------------------------------------------------------------------
-- CopyInfo

data CopyInfo = CopyInfo
  { ciOffsets    :: V.Vector CInt
  , ciNumValues  :: CInt
  , ciValueSize  :: CInt
  , ciSwapBytes  :: CInt
  } deriving (Show)

ciNumOffsets :: CopyInfo -> CInt
ciNumOffsets = fromIntegral . V.length . ciOffsets

type SkipCopyOp = CInt

-- | Build copy instructions for the specified layout.
buildCopyInfo :: Layout -> CopyInfo
buildCopyInfo layout =
    CopyInfo { ciOffsets, ciNumValues, ciValueSize, ciSwapBytes }
  where
    ciNumValues = copySize `quot` ciValueSize
    ciValueSize = fromIntegral (L.valueSize1 layout)
    ciSwapBytes = if needsByteSwap layout then 1 else 0

    (copySize, ciOffsets) = (splitOps . optimize . toSkipCopyOps) layout

    -- convert from layout to skip/copy operations
    toSkipCopyOps :: Layout -> V.Vector SkipCopyOp
    toSkipCopyOps = go
      where
        go v@(Value _)   = V.singleton (copyOp (L.valueSize1 v))
        go (Offset n xs) = skipOp n `V.cons` go xs
        go (Repeat n xs) = V.concat (replicate n (go xs))
        go (Group  n xs) = go xs `V.snoc` skipOp (n - L.size xs)

        -- positive number means skip 'n' bytes
        skipOp = fromIntegral

        -- negative number means copy 'n' bytes
        copyOp n = fromIntegral (-n)


    -- squash copies and skips together, remove no-ops
    optimize :: V.Vector SkipCopyOp -> V.Vector SkipCopyOp
    optimize = skips
      where
        skips  = sumWhile (> 0) copies
        copies = sumWhile (<= 0) skips

        sumWhile p k xs
            | V.null xs = V.empty
            | otherwise = let (ys, zs) = V.span p xs
                          in case V.sum ys of
                              0 -> k zs
                              s -> s `V.cons` k zs

    -- ensures first and last operations are skips or no-ops,
    -- then strips all copies out returns a tuple of the form
    -- (copy size, skip operations)
    splitOps :: V.Vector SkipCopyOp -> (CInt, V.Vector CInt)
    splitOps = split . head0 . last0
      where
        head0 xs | V.head xs < 0 = 0 `V.cons` xs
                 | otherwise     = xs

        last0 xs | V.last xs < 0 = xs `V.snoc` 0
                 | otherwise     = xs

        split :: V.Vector SkipCopyOp -> (CInt, V.Vector CInt)
        split xs = (-copyOp, V.filter isSkip xs)
          where
            copyOp = V.head (V.dropWhile (>= 0) xs)

            isSkip x | x == copyOp = False -- remove
                     | x >= 0      = True  -- keep
                     | otherwise   = error $
                         "buildCopyInfo: invalid copy operation " ++
                         "(expected <" ++ show (-copyOp) ++ " bytes>," ++
                         " actual <" ++ show (-x) ++ " bytes>)"


------------------------------------------------------------------------
-- Endian check

needsByteSwap :: Layout -> Bool
needsByteSwap x = case L.byteOrder x of
    NoByteOrder  -> False
    LittleEndian -> hostIsBigEndian
    BigEndian    -> hostIsLittleEndian

endianCheck :: Word8
endianCheck = unsafePerformIO $ alloca $ \p -> do
    poke p (0x01020304 :: Word32)
    peek (castPtr p :: Ptr Word8)

hostIsLittleEndian :: Bool
hostIsLittleEndian = endianCheck == 4

hostIsBigEndian :: Bool
hostIsBigEndian = endianCheck == 1


------------------------------------------------------------------------
-- FFI

type CodecFn =
       Ptr Word8 -- ^ destination memory area
    -> Ptr Word8 -- ^ source memory area
    -> CInt      -- ^ number of times to repeat the encode/decode
    -> CInt      -- ^ number of skip operations in 'offsets'
    -> Ptr CInt  -- ^ offset / skip list
    -> CInt      -- ^ number of values to copy in between each skip
    -> CInt      -- ^ size of a single value in bytes
    -> CInt      -- ^ non-zero to swap the byte order of values
    -> IO CInt   -- ^ zero on success, non-zero otherwise

foreign import ccall unsafe "data_layout_encode"
    c_encode :: CodecFn

foreign import ccall unsafe "data_layout_decode"
    c_decode :: CodecFn
