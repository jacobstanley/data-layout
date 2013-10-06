-- | A DSL for describing data laid out in a regular fashion with
-- a combination of structures and arrays.

module Data.Layout (
      Layout
    , Bytes
    , Reps
    , ByteOrder(..)
    , module Data.Layout.Language
    , module Data.Layout.Vector
    ) where

import Data.Layout.Internal
import Data.Layout.Language
import Data.Layout.Vector
