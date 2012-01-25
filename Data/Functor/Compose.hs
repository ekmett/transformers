-- |
-- Module      :  Data.Functor.Compose
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Composition of functors.

module Data.Functor.Compose (
    Compose(..),
   ) where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

-- | Right-to-left composition of functors.
-- The composition of applicative functors is always applicative,
-- but the composition of monads is not always a monad.
newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
    empty = Compose empty
    Compose x <|> Compose y = Compose (x <|> y)
