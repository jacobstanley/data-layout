{-# LANGUAGE CPP #-}

module Data.Layout.ForeignPtr
    ( mallocPlainForeignPtrBytes
    ) where

#if __GLASGOW_HASKELL__ >= 605

import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

#else

import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes)

mallocPlainForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytes = mallocForeignPtrBytes

#endif
