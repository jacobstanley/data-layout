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
import           Data.List (nub)
import qualified Data.Vector.Storable as V
import           Data.Word (Word8, Word32)
import           Foreign.C (CInt)
import           Foreign.ForeignPtr ()
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import           Foreign.Marshal (with)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, plusPtr, castPtr)
import           Foreign.Storable (Storable, peek, poke, sizeOf)
import           System.IO.Unsafe (unsafePerformIO)

import           Data.Layout.ForeignPtr (mallocPlainForeignPtrBytes)
import qualified Data.Layout.Language as L
import           Data.Layout.Types (Layout(..), ByteOrder(..))

import           Debug.Trace

------------------------------------------------------------------------
-- Vector Operations

data StorableVector where
    SV :: Storable a => V.Vector a -> StorableVector

encodeVectors :: [(Codec, StorableVector)] -> ByteString
encodeVectors [] = B.empty
encodeVectors xs = unsafePerformIO $ do
    -- check all codecs produce the same size output
    when (uniqueSizeCount /= 1) (error sizeErrMsg)

    -- check that each vector type matches its codec
    mapM_ check xs

    return B.empty
  where
    sizes@(bstrBytes:_) = map (encodedSize . fst) xs
    uniqueSizes         = nub sizes
    uniqueSizeCount     = length uniqueSizes

    reps@(n:_) = map go xs
    go (c, SV v) = valueCount c `quot` V.length v

    check (c, SV v) = checkValueSize "encodeVectors" c v

    sizeErrMsg = concat
      [ "Data.Layout.Vector.encodeVectors: "
      , "The codecs specified do not have data layouts with the same "
      , "encoded size. ", show uniqueSizeCount, " different sizes "
      , show uniqueSizes, " were found when verifying the encoded size "
      , "of each codec: ", show sizes ]

    -- go :: (Codec, StorableVector) -> ByteString
    -- go (c, SV (v :: V.Vector a)) = trace msg (B.pack [65,66,67])
    --   where
    --     msg = "elems = " ++ show (V.length v) ++
    --           ", elemSize = " ++ show (sizeOf (undefined :: a))

decodeVector :: forall a. Storable a => Codec -> ByteString -> V.Vector a
decodeVector codec bstr@(PS bp bpOff _) = unsafePerformIO $ do
    -- ensure the size of the bytestring matches the codec
    when (leftover /= 0) (error totalSizeErrMsg)

    -- check that the vector type matches the codec
    checkValueSize "decodeVector" codec (V.empty :: V.Vector a)

    -- allocate memory for the vector
    vp <- mallocPlainForeignPtrBytes vectorBytes

    -- decode the bytestring in to the vector
    decode codec n (DstPtr vp 0) (SrcPtr bp bpOff)

    -- return vector
    return (V.unsafeFromForeignPtr0 (castForeignPtr vp) vectorElems)
  where
    requiredBytes = encodedSize codec
    bstrBytes     = B.length bstr

    (n, leftover) = bstrBytes `quotRem` requiredBytes

    vectorBytes   = n * decodedSize codec
    vectorElems   = n * valueCount codec

    totalSizeErrMsg = concat
      [ "Data.Layout.Vector.decodeVector: "
      , "The source ByteString is not a multiple of "
      , show requiredBytes, " bytes, as required by the Codec. "
      , show bstrBytes, " bytes were provided, which leaves "
      , show leftover, " bytes unused." ]

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


------------------------------------------------------------------------
-- Ptr Operations

data DstPtr = DstPtr {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int

data SrcPtr = SrcPtr {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int

data Codec = Codec
    { decode      :: Int -> DstPtr -> SrcPtr -> IO ()
    , encodedSize :: Int
    , decodedSize :: Int
    , valueCount  :: Int
    , valueSize   :: Int
    }

-- | Copies data from the second memory area (source) into the first
-- memory area (destination) using the specified layout.
compile :: Layout -> Codec
compile layout = Codec
    { decode      = \n -> runLayoutCopy n (buildCopyInfo layout)
    , encodedSize = L.size layout
    , decodedSize = L.valueSizeN layout
    , valueCount  = L.valueCount layout
    , valueSize   = L.valueSize1 layout
    }

runLayoutCopy :: Int -> CopyInfo -> DstPtr -> SrcPtr -> IO ()
runLayoutCopy reps info (DstPtr dstFP dstOff) (SrcPtr srcFP srcOff) =
    -- unbox foreign pointers for use
    withForeignPtr dstFP $ \dst0 -> do
    withForeignPtr srcFP $ \src0 -> do

    -- add the offset
    let dst = dst0 `plusPtr` dstOff
        src = src0 `plusPtr` srcOff

    -- get a pointer to the offsets
    V.unsafeWith (ciOffsets info) $ \offsets -> do

    -- decode the data
    err <- c_decode
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
      1 -> error ("runLayoutCopy: invalid value size: " ++ show (ciValueSize info))
      _ -> error "runLayoutCopy: unknown error"


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

foreign import ccall unsafe "data_layout_decode"
    c_decode :: Ptr Word8 -- (in) destination memory area
             -> Ptr Word8 -- (in) source memory area
             -> CInt      -- (in) number of times to repeat the decode
             -> CInt      -- (in) number of skip operations in 'offsets'
             -> Ptr CInt  -- (in) offset / skip list
             -> CInt      -- (in) number of values to copy in between each skip
             -> CInt      -- (in) size of a single value in bytes
             -> CInt      -- (in) non-zero to swap the byte order of values
             -> IO CInt   -- (ret) zero on success, non-zero otherwise
