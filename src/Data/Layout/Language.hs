-- | A DSL for describing data laid out in a regular fashion with
-- a combination of structures and arrays.

module Data.Layout.Language (
    -- * Data types
      Layout (..)
    , Bytes

    -- * Primitives
    , repeat
    , offset
    , group
    , value

    -- * Optimizations
    , optimize

    -- * Utility
    , modifyInner
    , lsize
    , tsize
    , vsize
    , vcount
    , chunk

    ) where

import Prelude hiding (repeat)

------------------------------------------------------------------------
-- Data types

type Bytes = Int

data Layout
    = Value  Bytes
    | Offset Bytes Layout
    | Group  Bytes Layout
    | Repeat Int   Layout
    deriving (Show, Eq)


------------------------------------------------------------------------
-- Primitives

repeat :: Int -> Layout -> Layout
repeat n | n > 1     = Repeat n
         | otherwise = id

offset :: Int -> Layout -> Layout
offset n | n > 0     = Offset n
         | otherwise = id

group :: Int -> Layout -> Layout
group n | n > 0     = Group n
        | otherwise = error "group: must request 1 or more bytes"

value :: Int -> Layout
value n | n > 0     = Value n
        | otherwise = error "value: must request 1 or more bytes"


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

-- | Calculates the total size of the layout.
lsize :: Layout -> Int
lsize (Value  x)       = x
lsize (Offset x inner) = x + lsize inner
lsize (Group  x _)     = x
lsize (Repeat n inner) = n * lsize inner

-- | Calculates the total size of the values stored in the layout.
tsize :: Layout -> Int
tsize (Value  x)       = x
tsize (Offset _ inner) = tsize inner
tsize (Group  _ inner) = tsize inner
tsize (Repeat n inner) = tsize inner * n

-- | Calculates the size of a single value in the layout.
vsize :: Layout -> Int
vsize (Value  x)       = x
vsize (Offset _ inner) = vsize inner
vsize (Group  _ inner) = vsize inner
vsize (Repeat _ inner) = vsize inner

-- | Counts the number of times a value is repeated in the layout.
vcount :: Layout -> Int
vcount (Value  _)       = 1
vcount (Offset _ inner) = vcount inner
vcount (Group  _ inner) = vcount inner
vcount (Repeat n inner) = vcount inner * n

chunk :: Int -> Layout -> [Layout]
chunk n (Repeat m inner) = replicate (m `div` n) (Repeat n inner) ++ [Repeat (m `rem` n) inner]
chunk _ layout           = [layout]
