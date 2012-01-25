-- |
-- Module      :  Data.Functor.Product
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Products, lifted to functors.

module Data.Functor.Product (
    Product(..),
   ) where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (mappend)
import Data.Traversable (Traversable(traverse))

-- | Lifted product of functors.
data Product f g a = Pair (f a) (g a)

instance (Functor f, Functor g) => Functor (Product f g) where
    fmap f (Pair x y) = Pair (fmap f x) (fmap f y)

instance (Foldable f, Foldable g) => Foldable (Product f g) where
    foldMap f (Pair x y) = foldMap f x `mappend` foldMap f y

instance (Traversable f, Traversable g) => Traversable (Product f g) where
    traverse f (Pair x y) = Pair <$> traverse f x <*> traverse f y

instance (Applicative f, Applicative g) => Applicative (Product f g) where
    pure x = Pair (pure x) (pure x)
    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)

instance (Alternative f, Alternative g) => Alternative (Product f g) where
    empty = Pair empty empty
    Pair x1 y1 <|> Pair x2 y2 = Pair (x1 <|> x2) (y1 <|> y2)
