-- | A DSL for describing data laid out in a regular fashion with
-- a combination of structures and arrays.

module Data.Layout (
      module Data.Layout.Language
    , module Data.Layout.Vector
    ) where

import Data.Layout.Language (Layout)
import Data.Layout.Language hiding (Layout (..), LayoutWord (..))
import Data.Layout.Vector
