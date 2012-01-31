module Data.Layout.Types where

type Bytes = Int
type Reps = Int

data Layout =
    Value  ValueFormat  -- ^ A value with a known format.
  | Offset Bytes Layout -- ^ Skip 'n' bytes in to a struct
  | Group  Bytes Layout -- ^ A struct which is 'n' bytes in size
  | Repeat Reps  Layout -- ^ An array with 'n' elements
  deriving (Eq, Show)

data ValueFormat =
    Word8    -- ^ 8-bit word.
  | Word16le -- ^ 16-bit word, little endian.
  | Word32le -- ^ 32-bit word, little endian.
  | Word64le -- ^ 64-bit word, little endian.
  | Word16be -- ^ 16-bit word, big endian.
  | Word32be -- ^ 32-bit word, big endian.
  | Word64be -- ^ 64-bit word, big endian.
  deriving (Eq, Show)

data ByteOrder =
    NoByteOrder  -- ^ The word is only one byte in size.
  | LittleEndian -- ^ The least significant byte is first in the word.
  | BigEndian    -- ^ The most significant byte is first in the word.
  deriving (Eq, Show)
