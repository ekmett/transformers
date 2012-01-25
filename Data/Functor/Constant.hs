-- |
-- Module      :  Data.Functor.Constant
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The constant functor.

module Data.Functor.Constant (
    Constant(..),
   ) where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))

-- | Constant functor.
newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
    fmap f (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap f (Constant x) = mempty

instance Traversable (Constant a) where
    traverse f (Constant x) = pure (Constant x)

instance (Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant x <*> Constant y = Constant (x `mappend` y)
