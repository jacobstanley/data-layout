-- | /Use the internal representation of layouts at your own risk./
--
-- The smart constructors in "Data.Layout.Language" enforce the
-- documented invariants for `Layout`. The library has undefined
-- behavior if these invariants are violated.
--
-- The recommended way to use this library is to import "Data.Layout".

module Data.Layout.Internal where

-- | Size in bytes.
type Bytes = Int

-- | Number of repetitions.
type Reps = Int

-- | Describes the binary layout of a set of data.
data Layout =
    -- | A value with a known format.
    Value ValueFormat

    -- | Skip 'n' bytes in to a struct.
    -- /The offset must be 1 or more bytes./
  | Offset Bytes Layout

    -- | A struct which is 'n' bytes in size.
    -- /The size of the struct must be larger than that of the field layout it wraps./
  | Group Bytes Layout

    -- | An array with 'n' elements.
    -- /An array must have 2 or more elements./
  | Repeat Reps Layout
  deriving (Eq, Show)

-- | The size and byte order of a value.
data ValueFormat =
    Word8    -- ^ 8-bit word.
  | Word16le -- ^ 16-bit word, little endian.
  | Word32le -- ^ 32-bit word, little endian.
  | Word64le -- ^ 64-bit word, little endian.
  | Word16be -- ^ 16-bit word, big endian.
  | Word32be -- ^ 32-bit word, big endian.
  | Word64be -- ^ 64-bit word, big endian.
  deriving (Eq, Show)

-- | The byte order of a value.
data ByteOrder =
    NoByteOrder  -- ^ The word is only one byte in size.
  | LittleEndian -- ^ The least significant byte is first in the word.
  | BigEndian    -- ^ The most significant byte is first in the word.
  deriving (Eq, Show)
