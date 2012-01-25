-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Identity
-- Copyright   :  (c) 2007 Magnus Therning
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The identity monad transformer.
--
-- This is useful for functions parameterized by a monad transformer.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Identity (
    -- * The identity monad transformer
    IdentityT(..),
    mapIdentityT,
    -- * Lifting other operations
    liftCatch,
    liftCallCC,
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))

-- | The trivial monad transformer, which maps a monad to an equivalent monad.
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance (Functor m) => Functor (IdentityT m) where
    fmap f = mapIdentityT (fmap f)

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    (<*>) = lift2IdentityT (<*>)
 
instance (Alternative m) => Alternative (IdentityT m) where
    empty = IdentityT empty
    (<|>) = lift2IdentityT (<|>)

instance (Monad m) => Monad (IdentityT m) where
    return = IdentityT . return
    m >>= k = IdentityT $ runIdentityT . k =<< runIdentityT m
    fail msg = IdentityT $ fail msg
 
instance (MonadPlus m) => MonadPlus (IdentityT m) where
    mzero = IdentityT mzero
    mplus = lift2IdentityT mplus

instance (MonadIO m) => MonadIO (IdentityT m) where
    liftIO = IdentityT . liftIO

instance MonadTrans IdentityT where
    lift = IdentityT

-- | Lift a unary operation to the new monad.
mapIdentityT :: (m a -> n b) -> IdentityT m a -> IdentityT n b
mapIdentityT f = IdentityT . f . runIdentityT

-- | Lift a binary operation to the new monad.
lift2IdentityT ::
    (m a -> n b -> p c) -> IdentityT m a -> IdentityT n b -> IdentityT p c
lift2IdentityT f a b = IdentityT (f (runIdentityT a) (runIdentityT b))

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (((a -> m b) -> m a) ->
    m a) -> ((a -> IdentityT m b) -> IdentityT m a) -> IdentityT m a
liftCallCC callCC f =
    IdentityT $ callCC $ \ c -> runIdentityT (f (IdentityT . c))

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m a -> (e -> m a) -> m a) ->
    IdentityT m a -> (e -> IdentityT m a) -> IdentityT m a
liftCatch f m h = IdentityT $ f (runIdentityT m) (runIdentityT . h)
