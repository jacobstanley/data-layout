-- | A DSL for describing data laid out in a regular fashion with
-- a combination of structures and arrays.

module Data.Layout.Language (
    -- * Combinators
      repeat
    , offset
    , group

    -- * Value Formats
    , word8
    , word16le
    , word32le
    , word64le
    , word16be
    , word32be
    , word64be

    -- * Optimizations
    , optimize

    -- * Utility
    , size
    , byteOrder
    , valueSize1
    , valueSizeN
    , valueCount
    , chunk

    ) where

import Prelude hiding (repeat)

import Data.Layout.Types

------------------------------------------------------------------------
-- Combinators

-- | Repeat a layout 'n' times.
repeat :: Reps -> Layout -> Layout
repeat n | n > 1     = Repeat n
         | n == 1    = id
         | otherwise = invalid "repeat" "repetition must be at least 1"

-- | Skip 'n' bytes before accessing the next part of the layout.
offset :: Bytes -> Layout -> Layout
offset n | n > 0     = Offset n
         | n == 0    = id
         | otherwise = invalid "offset" "must skip 0 or more bytes"

-- | Wrap a layout with a group which is 'n' bytes in size.
group :: Bytes -> Layout -> Layout
group n xs | n <= 0      = invalid "group" "must contain 1 or more bytes"
           | n < size xs = invalid "group" "cannot be smaller than the inner layout"
           | otherwise   = Group n xs

invalid :: String -> String -> a
invalid fn msg = error ("Data.Layout.Language." ++ fn ++ ": " ++ msg)


------------------------------------------------------------------------
-- Value Formats

-- | 8-bit word.
word8 :: Layout
word8 = Value Word8

-- | 16-bit word, little endian.
word16le :: Layout
word16le = Value Word16le

-- | 32-bit word, little endian.
word32le :: Layout
word32le = Value Word32le

-- | 64-bit word, little endian.
word64le :: Layout
word64le = Value Word64le

-- | 16-bit word, big endian.
word16be :: Layout
word16be = Value Word16be

-- | 32-bit word, big endian.
word32be :: Layout
word32be = Value Word32be

-- | 64-bit word, big endian.
word64be :: Layout
word64be = Value Word64be

formatSize :: ValueFormat -> Bytes
formatSize Word8    = 1
formatSize Word16le = 2
formatSize Word32le = 4
formatSize Word64le = 8
formatSize Word16be = 2
formatSize Word32be = 4
formatSize Word64be = 8

formatByteOrder :: ValueFormat -> ByteOrder
formatByteOrder Word8    = NoByteOrder
formatByteOrder Word16le = LittleEndian
formatByteOrder Word32le = LittleEndian
formatByteOrder Word64le = LittleEndian
formatByteOrder Word16be = BigEndian
formatByteOrder Word32be = BigEndian
formatByteOrder Word64be = BigEndian

------------------------------------------------------------------------
-- Optimizations

data GroupStatus = NoGroup | GotGroup

-- | Removes redundant data groups. Grouping a set of data is only
-- sensible when the data can be repeated. If we find multiple
-- groupings without a repeat in the middle we can remove all but
-- the outer group.
optimizeGroups :: Layout -> Layout
optimizeGroups = go NoGroup
  where
    -- found a group, pass GotGroup to inner layout
    go NoGroup  (Group n xs) = Group n (go GotGroup xs)

    -- already got an outer group, drop this one as it is redundant
    go GotGroup (Group _ xs) = go GotGroup xs

    -- reset group status, further grouping may be required for repeats
    go _       (Repeat n xs) = Repeat n (go NoGroup xs)

    -- other configurations are uninteresting for this optimization
    go status x              = mapInner (go status) x

-- | Folds adjancent data offset operations in to a single one.
optimizeOffsets :: Layout -> Layout
optimizeOffsets = go
  where
    go (Offset n (Offset m xs)) = go (Offset (n + m) xs)
    go x                        = mapInner go x

-- | Remove redundancies from a layout.
optimize :: Layout -> Layout
optimize = optimizeOffsets . optimizeGroups


------------------------------------------------------------------------
-- Utility

-- | Calculates the total size of the layout.
size :: Layout -> Int
size (Value  x)       = formatSize x
size (Offset x xs) = x + size xs
size (Group  x _)     = x
size (Repeat n xs) = n * size xs

-- | Gets the format of a single value in the layout.
valueFormat :: Layout -> ValueFormat
valueFormat (Value fmt)   = fmt
valueFormat (Offset _ xs) = valueFormat xs
valueFormat (Group  _ xs) = valueFormat xs
valueFormat (Repeat _ xs) = valueFormat xs

-- | Gets the byte order of the values in the layout.
byteOrder :: Layout -> ByteOrder
byteOrder = formatByteOrder . valueFormat

-- | Calculates the size of a single value in the layout.
valueSize1 :: Layout -> Int
valueSize1 = formatSize . valueFormat

-- | Calculates the total size of the values stored in the layout.
valueSizeN :: Layout -> Int
valueSizeN x = valueSize1 x * valueCount x

-- | Counts the number of times a value is repeated in the layout.
valueCount :: Layout -> Int
valueCount (Value  _)    = 1
valueCount (Offset _ xs) = valueCount xs
valueCount (Group  _ xs) = valueCount xs
valueCount (Repeat n xs) = valueCount xs * n

-- | Applys a transformation to the inner layout.
mapInner :: (Layout -> Layout) -> Layout -> Layout
mapInner _ (Value fmt)   = Value fmt
mapInner f (Offset n xs) = Offset n (f xs)
mapInner f (Group  n xs) = Group  n (f xs)
mapInner f (Repeat n xs) = Repeat n (f xs)

-- | Splits a repeated layout in to multiple layouts each with a maximum
-- of 'n' repetitions.
chunk :: Int -> Layout -> [Layout]
chunk n (Repeat m xs) = replicate (m `div` n) (Repeat n xs) ++ [Repeat (m `rem` n) xs]
chunk _ xs            = [xs]
