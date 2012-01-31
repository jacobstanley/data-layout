-- | A DSL for describing data laid out in a regular fashion with
-- a combination of structures and arrays.

module Data.Layout.Language (
    -- * Data types
      Bytes
    , Endian (..)
    , LayoutWord (..)
    , Layout (..)

    -- * Combinators
    , repeat
    , offset
    , group

    -- * Values
    , word8
    , word16le
    , word32le
    , word64le
    , word16be
    , word32be
    , word64be
    , wsize
    , wendian

    -- * Optimizations
    , optimize

    -- * Utility
    , modifyInner
    , withValue
    , size
    , vsizeAll
    , vsize1
    , vcount
    , chunk

    ) where

import Prelude hiding (repeat)

------------------------------------------------------------------------
-- Data types

type Bytes = Int

data Endian = None | LittleEndian | BigEndian
  deriving (Eq, Show)

data LayoutWord =
      Word8    -- ^ 8-bit word.
    | Word16le -- ^ 16-bit word, little endian.
    | Word32le -- ^ 32-bit word, little endian.
    | Word64le -- ^ 64-bit word, little endian.
    | Word16be -- ^ 16-bit word, big endian.
    | Word32be -- ^ 32-bit word, big endian.
    | Word64be -- ^ 64-bit word, big endian.
  deriving (Eq, Show)

data Layout =
      Value  LayoutWord   -- ^ The size / byte order of a value.
    | Offset Bytes Layout -- ^ Skip n bytes before applying the inner layout.
    | Group  Bytes Layout -- ^ Isolate n bytes to allow repetition.
    | Repeat Int   Layout -- ^ Repeat the inner layout n times.
  deriving (Show, Eq)


------------------------------------------------------------------------
-- Combinators

repeat :: Int -> Layout -> Layout
repeat n | n > 1     = Repeat n
         | otherwise = id

offset :: Int -> Layout -> Layout
offset n | n > 0     = Offset n
         | otherwise = id

group :: Int -> Layout -> Layout
group n | n > 0     = Group n
        | otherwise = error "group: must request 1 or more bytes"


------------------------------------------------------------------------
-- Values

word8 :: Layout
word8 = Value Word8

word16le :: Layout
word16le = Value Word16le

word32le :: Layout
word32le = Value Word32le

word64le :: Layout
word64le = Value Word64le

word16be :: Layout
word16be = Value Word16be

word32be :: Layout
word32be = Value Word32be

word64be :: Layout
word64be = Value Word64be

wsize :: LayoutWord -> Int
wsize Word8    = 1
wsize Word16le = 2
wsize Word32le = 4
wsize Word64le = 8
wsize Word16be = 2
wsize Word32be = 4
wsize Word64be = 8

wendian :: LayoutWord -> Endian
wendian Word8    = None
wendian Word16le = LittleEndian
wendian Word32le = LittleEndian
wendian Word64le = LittleEndian
wendian Word16be = BigEndian
wendian Word32be = BigEndian
wendian Word64be = BigEndian

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
    go NoGroup  (Group n inner) = Group n (go GotGroup inner)

    -- already got an outer group, drop this one as it is redundant
    go GotGroup (Group _ inner) = go GotGroup inner

    -- reset group status, further grouping may be required for repeats
    go _       (Repeat n inner) = Repeat n (go NoGroup inner)

    -- other configurations are uninteresting for this optimization
    go status x                 = modifyInner (go status) x

-- | Folds adjancent data offset operations in to a single one.
optimizeOffsets :: Layout -> Layout
optimizeOffsets = go
  where
    go (Offset n (Offset m inner)) = go (Offset (n + m) inner)
    go x                           = modifyInner go x

-- | Remove redundancies from a layout.
optimize :: Layout -> Layout
optimize = optimizeOffsets . optimizeGroups


------------------------------------------------------------------------
-- Utility

-- | Maps a modification over the inner layout.
modifyInner :: (Layout -> Layout) -> Layout -> Layout
modifyInner _ (Value  n)       = Value  n
modifyInner f (Offset n inner) = Offset n (f inner)
modifyInner f (Group  n inner) = Group  n (f inner)
modifyInner f (Repeat n inner) = Repeat n (f inner)

-- | Maps a function over the inner value.
withValue :: (LayoutWord -> a) -> Layout -> a
withValue f (Value  x)       = f x
withValue f (Offset _ inner) = withValue f inner
withValue f (Group  _ inner) = withValue f inner
withValue f (Repeat _ inner) = withValue f inner

-- | Calculates the total size of the layout.
size :: Layout -> Int
size (Value  x)       = wsize x
size (Offset x inner) = x + size inner
size (Group  x _)     = x
size (Repeat n inner) = n * size inner

-- | Calculates the total size of the values stored in the layout.
vsizeAll :: Layout -> Int
vsizeAll x = vsize1 x * vcount x

-- | Calculates the size of a single value in the layout.
vsize1 :: Layout -> Int
vsize1 = withValue wsize

-- | Counts the number of times a value is repeated in the layout.
vcount :: Layout -> Int
vcount (Value  _)       = 1
vcount (Offset _ inner) = vcount inner
vcount (Group  _ inner) = vcount inner
vcount (Repeat n inner) = vcount inner * n

chunk :: Int -> Layout -> [Layout]
chunk n (Repeat m inner) = replicate (m `div` n) (Repeat n inner) ++ [Repeat (m `rem` n) inner]
chunk _ layout           = [layout]
