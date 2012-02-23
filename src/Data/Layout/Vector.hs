{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Layout.Vector (
      Codec
    , compile
    , readVector
    ) where

import           Control.Monad (when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Vector.Storable as V
import           Data.Word (Word8, Word32)
import           Foreign.C (CInt)
import           Foreign.ForeignPtr (mallocForeignPtrBytes)
import           Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import           Foreign.Marshal (with)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, plusPtr, castPtr)
import           Foreign.Storable (Storable, peek, poke)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.Layout.Language as L
import           Data.Layout.Types (Layout(..), ByteOrder(..))

------------------------------------------------------------------------

readVector :: Storable a => Int -> Codec -> ByteString -> V.Vector a
readVector n codec bs@(PS bfp off _) =
    unsafePerformIO $ do

    -- ensure we have a big enough bytestring to fulfill the layout
    when (bsActualSize < bsRequiredSize) (error sizeErrMsg)

    -- allocate memory for the vector
    vfp <- mallocForeignPtrBytes bytes

    -- unbox foreign pointers for use
    withForeignPtr bfp $ \bpOrig -> do
    withForeignPtr vfp $ \vp -> do

    -- add the bytestring offset
    let bp = bpOrig `plusPtr` off

    -- do data transfer
    decode codec n (DstSrc vp bp)

    -- return vector
    return (V.unsafeFromForeignPtr (castForeignPtr vfp) 0 elems)
  where
    bytes = n * decodedSize codec
    elems = n * valueCount codec

    bsActualSize   = B.length bs
    bsRequiredSize = n * encodedSize codec

    sizeErrMsg =
        "Data.Layout.Vector.readVector: The source ByteString " ++
        "is too small to hold the data specified by the layout. " ++
        show bsRequiredSize ++ " bytes are required, but only " ++
        show bsActualSize ++ " bytes were provided."

------------------------------------------------------------------------

data DstSrc = DstSrc {-# UNPACK #-} !(Ptr Word8)
                     {-# UNPACK #-} !(Ptr Word8)

data Codec = Codec
    { decode      :: Int -> DstSrc -> IO DstSrc
    , encodedSize :: Int
    , decodedSize :: Int
    , valueCount  :: Int
    }

-- | Copies data from the second memory area (source) into the first
-- memory area (destination) using the specified layout.
compile :: Layout -> Codec
compile layout = Codec
    { decode      = \n -> runLayoutCopy n (buildCopyInfo layout)
    , encodedSize = L.size layout
    , decodedSize = L.valueSizeN layout
    , valueCount  = L.valueCount layout
    }

runLayoutCopy :: Int -> CopyInfo -> DstSrc -> IO DstSrc
runLayoutCopy reps info (DstSrc dst src) =
    with dst $ \pDst ->
    with src $ \pSrc ->
    V.unsafeWith (ciOffsets info) $ \pOffsets -> do

    -- this mutates pDst and pSrc
    err <- c_copy
        pDst pSrc
        (fromIntegral reps)
        (ciNumOffsets info)
        pOffsets
        (ciNumValues info)
        (ciValueSize info)
        (ciSwapBytes info)

    case err of
      0 -> return ()
      1 -> error ("runLayoutCopy: invalid value size: " ++ show (ciValueSize info))
      _ -> error "runLayoutCopy: unknown error"

    -- pDst and pSrc now contain the
    -- new dst/src locations
    dst' <- peek pDst
    src' <- peek pSrc

    return (DstSrc dst' src')


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

foreign import ccall unsafe "data_layout_copy"
    c_copy :: Ptr (Ptr Word8) -- (in/out) destination memory area
           -> Ptr (Ptr Word8) -- (in/out) source memory area
           -> CInt     -- (in) number of times to repeat the copy instructions
           -> CInt     -- (in) number of skip operations in 'offsets'
           -> Ptr CInt -- (in) offset / skip list
           -> CInt     -- (in) number of values to copy in between each skip
           -> CInt     -- (in) size of a single value in bytes
           -> CInt     -- (in) non-zero to swap the byte order of values
           -> IO CInt  -- (ret) zero on success, non-zero otherwise
