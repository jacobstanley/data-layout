{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Layout.Vector (
      Transformer
    , newTransformer
    , readVector
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Vector.Storable as V
import           Data.Word (Word8)
import           Foreign.C (CInt)
import           Foreign.ForeignPtr (mallocForeignPtrBytes)
import           Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import           Foreign.Marshal (with)
import           Foreign.Ptr (Ptr, plusPtr)
import           Foreign.Storable (Storable, peek)
import           System.IO.Unsafe (unsafePerformIO)

import           Data.Layout.Language (Layout (..), lsize, tsize, vsize, vcount)

------------------------------------------------------------------------

readVector :: Storable a => Int -> Transformer -> ByteString -> V.Vector a
readVector n transformer (PS bfp off _) =
    unsafePerformIO $ do

    -- allocate memory for the vector
    vfp <- mallocForeignPtrBytes bytes

    -- unbox foreign pointers for use
    withForeignPtr bfp $ \bpOrig -> do
    withForeignPtr vfp $ \vp -> do

    -- add the bytestring offset
    let bp = bpOrig `plusPtr` off

    -- TODO: check that bytestring is long enough

    -- do data transfer
    runT transformer n (DstSrc vp bp)

    -- return vector
    return (V.unsafeFromForeignPtr (castForeignPtr vfp) 0 elems)
  where
    bytes = n * sizeT transformer
    elems = n * countT transformer

------------------------------------------------------------------------

data DstSrc = DstSrc {-# UNPACK #-} !(Ptr Word8)
                     {-# UNPACK #-} !(Ptr Word8)

data Transformer = Transformer
    { runT   :: Int -> DstSrc -> IO DstSrc
    , sizeT  :: Int
    , countT :: Int
    }

-- | Copies data from the second memory area (source) into the first
-- memory area (destination) using the specified layout.
newTransformer :: Layout -> Transformer
newTransformer layout = Transformer
    { runT   = \n -> runLayoutCopy n (vsize layout) (layoutOps layout)
    , sizeT  = tsize layout
    , countT = vcount layout
    }

runLayoutCopy :: Int -> Int -> V.Vector LayoutOp -> DstSrc -> IO DstSrc
runLayoutCopy reps valSize layout (DstSrc dst src) =
    with dst $ \pDst ->
    with src $ \pSrc ->
    V.unsafeWith offsets $ \pOffsets -> do

    -- this mutates pDst and pSrc
    err <- c_copy
        pDst pSrc
        numReps
        numOffsets pOffsets
        numValues valueSize
        swapBytes

    case err of
      0 -> return ()
      1 -> error ("runLayoutCopy: invalid value size: " ++ show valueSize)
      _ -> error "runLayoutCopy: unknown error"

    -- pDst and pSrc now contain the
    -- new dst/src locations
    dst' <- peek pDst
    src' <- peek pSrc

    return (DstSrc dst' src')
  where
    numReps    = fromIntegral reps
    numOffsets = fromIntegral (V.length offsets)
    offsets    = V.tail layout
    numValues  = fromIntegral (fromIntegral (V.head layout) `quot` valSize)
    valueSize  = fromIntegral valSize
    swapBytes  = 1

------------------------------------------------------------------------

type LayoutOp = CInt

-- | Builds a set of layout instructions for the specified layout.
layoutOps :: Layout -> V.Vector LayoutOp
layoutOps = fix . optimize . fromLayout
  where

    -- ensures first and last operations are drops or no-ops,
    -- then strips all copies out and replaces them with a
    -- single value at the start of the list
    fix :: V.Vector LayoutOp -> V.Vector LayoutOp
    fix = fixCopies . fixHead . fixLast
      where
        fixHead xs | V.head xs < 0 = 0 `V.cons` xs
                   | otherwise     = xs

        fixLast xs | V.last xs < 0 = xs `V.snoc` 0
                   | otherwise     = xs

        fixCopies :: V.Vector LayoutOp -> V.Vector LayoutOp
        fixCopies xs = copySize `V.cons` V.filter f xs
          where
            copySize  = negate copyOpVal
            copyOpVal = V.head $ V.dropWhile (>= 0) xs

            f x | x == copyOpVal = False -- remove
                | x >= 0         = True  -- keep
                | otherwise      = error $
                    "layoutOps: invalid copy operation " ++
                    "(expected <" ++ show copySize ++ " bytes>," ++
                    " actual <" ++ show (-x) ++ " bytes>)"


    -- squash copies and drops together, remove no-ops
    optimize :: V.Vector LayoutOp -> V.Vector LayoutOp
    optimize = drops
      where
        drops  = sumWhile (> 0) copies
        copies = sumWhile (<= 0) drops

        sumWhile f r' xs
            | V.null xs = V.empty
            | otherwise = let (ys, zs) = V.span f xs
                          in case V.sum ys of
                              0 -> r' zs
                              s -> s `V.cons` r' zs

    -- convert from layout to layout operations
    fromLayout :: Layout -> V.Vector LayoutOp
    fromLayout (Value  n)       = V.singleton (copyOp n)
    fromLayout (Offset n inner) = dropOp n `V.cons` fromLayout inner
    fromLayout (Repeat n inner) = V.concat $ replicate n $ fromLayout inner
    fromLayout (Group  n inner) = fromLayout inner `V.snoc` dropOp (n - lsize inner)

    -- drops are positive, copies are negative
    dropOp   = fromIntegral
    copyOp n = fromIntegral (-n)

------------------------------------------------------------------------

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
