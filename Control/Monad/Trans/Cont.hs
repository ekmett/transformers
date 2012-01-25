-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Cont
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Continuation monads.
--
-----------------------------------------------------------------------------

module Control.Monad.Trans.Cont (
    -- * The Cont monad
    Cont,
    cont,
    runCont,
    mapCont,
    withCont,
    -- * The ContT monad transformer
    ContT(..),
    mapContT,
    withContT,
    callCC,
    -- * Lifting other operations
    liftLocal,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

import Control.Applicative
import Control.Monad

{- |
Continuation monad.
@Cont r a@ is a CPS computation that produces an intermediate result
of type @a@ within a CPS computation whose final result type is @r@.

The @return@ function simply creates a continuation which passes the value on.

The @>>=@ operator adds the bound function into the continuation chain.
-}
type Cont r = ContT r Identity

-- | Construct a continuation-passing computation from a function.
-- (The inverse of 'runCont'.)
cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT (\ k -> Identity (f (runIdentity . k)))

-- | Runs a CPS computation, returns its result after applying the final
-- continuation to it.
-- (The inverse of 'cont'.)
runCont :: Cont r a	-- ^ continuation computation (@Cont@).
    -> (a -> r)		-- ^ the final continuation, which produces
			-- the final result (often 'id').
    -> r
runCont m k = runIdentity (runContT m (Identity . k))

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f = mapContT (Identity . f . runIdentity)

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f = withContT ((Identity .) . f . (runIdentity .))

{- |
The continuation monad transformer.
Can be used to add continuation handling to other monads.
-}
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f

instance Functor (ContT r m) where
    fmap f m = ContT $ \c -> runContT m (c . f)

instance Applicative (ContT r m) where
    pure a  = ContT ($ a)
    f <*> v = ContT $ \ k -> runContT f $ \ g -> runContT v (k . g)

instance Monad (ContT r m) where
    return a = ContT ($ a)
    m >>= k  = ContT $ \c -> runContT m (\a -> runContT (k a) c)

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = lift . liftIO

-- | @callCC@ (call-with-current-continuation) calls its argument
-- function, passing it the current continuation.  It provides
-- an escape continuation mechanism for use with continuation
-- monads.  Escape continuations one allow to abort the current
-- computation and return a value immediately.  They achieve a
-- similar effect to 'Control.Monad.Trans.Error.throwError'
-- and 'Control.Monad.Trans.Error.catchError' within an
-- 'Control.Monad.Trans.Error.ErrorT' monad.  The advantage of this
-- function over calling 'return' is that it makes the continuation
-- explicit, allowing more flexibility and better control.
--
-- The standard idiom used with @callCC@ is to provide a lambda-expression
-- to name the continuation. Then calling the named continuation anywhere
-- within its scope will escape from the computation, even if it is many
-- layers deep within nested computations.
callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \c -> runContT (f (\a -> ContT $ \_ -> c a)) c

-- | @'liftLocal' ask local@ yields a @local@ function for @'ContT' r m@.
liftLocal :: Monad m => m r' -> ((r' -> r') -> m r -> m r) ->
    (r' -> r') -> ContT r m a -> ContT r m a
liftLocal ask local f m = ContT $ \c -> do
    r <- ask
    local f (runContT m (local (const r) . c))
