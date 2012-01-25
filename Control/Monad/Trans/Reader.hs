-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Reader
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the 'ReaderT' monad transformer, which adds a static
-- environment to a given monad.
--
-- If the computation is to modify the stored information, use
-- "Control.Monad.Trans.State" instead.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Reader (
    -- * The Reader monad
    Reader,
    reader,
    runReader,
    mapReader,
    withReader,
    -- * The ReaderT monad transformer
    ReaderT(..),
    mapReaderT,
    withReaderT,
    -- * Reader operations
    ask,
    local,
    asks,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Instances ()

-- | The parameterizable reader monad.
--
-- Computations are functions of a shared environment.
--
-- The 'return' function ignores the environment, while @>>=@ passes
-- the inherited environment to both subcomputations.
type Reader r = ReaderT r Identity

-- | Constructor for computations in the reader monad.
reader :: (r -> a) -> Reader r a
reader f = ReaderT (Identity . f)

-- | Runs a @Reader@ and extracts the final value from it.
runReader :: Reader r a		-- ^ A @Reader@ to run.
    -> r			-- ^ An initial environment.
    -> a
runReader m = runIdentity . runReaderT m

-- | Transform the value returned by a @Reader@.
mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity)

-- | Execute a computation in a modified environment
-- (a specialization of 'withReaderT').
withReader
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> Reader r a       -- ^ Computation to run in the modified environment.
    -> Reader r' a
withReader = withReaderT

-- | The reader monad transformer,
-- which adds a read-only environment to the given monad.
--
-- The 'return' function ignores the environment, while @>>=@ passes
-- the inherited environment to both subcomputations.
newtype ReaderT r m a = ReaderT {
        -- | The underlying computation, as a function of the environment.
        runReaderT :: r -> m a
    }

-- | Transform the computation inside a @ReaderT@.
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

-- | Execute a computation in a modified environment
-- (a  more general version of 'local').
withReaderT
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Functor m) => Functor (ReaderT r m) where
    fmap f  = mapReaderT (fmap f)

instance (Applicative m) => Applicative (ReaderT r m) where
    pure    = liftReaderT . pure
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r

instance (Alternative m) => Alternative (ReaderT r m) where
    empty   = liftReaderT empty
    m <|> n = ReaderT $ \ r -> runReaderT m r <|> runReaderT n r

instance (Monad m) => Monad (ReaderT r m) where
    return   = lift . return
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = lift (fail msg)

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = lift mzero
    m `mplus` n = ReaderT $ \ r -> runReaderT m r `mplus` runReaderT n r

instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT $ \ r -> mfix $ \ a -> runReaderT (f a) r

instance MonadTrans (ReaderT r) where
    lift   = liftReaderT

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

-- | Fetch the value of the environment.
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

-- | Execute a computation in a modified environment
-- (a specialization of 'withReaderT').
local :: (Monad m)
    => (r -> r)         -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r m a
local = withReaderT

-- | Retrieve a function of the current environment.
asks :: (Monad m)
    => (r -> a)         -- ^ The selector function to apply to the environment.
    -> ReaderT r m a
asks f = liftM f ask

-- | Lift a @callCC@ operation to the new monad.
liftCallCC ::
    (((a -> m b) -> m a) -> m a)        -- ^ @callCC@ on the argument monad.
    -> ((a -> ReaderT r m b) -> ReaderT r m a) -> ReaderT r m a
liftCallCC callCC f = ReaderT $ \ r ->
    callCC $ \ c ->
    runReaderT (f (ReaderT . const . c)) r

-- | Lift a @catchError@ operation to the new monad.
liftCatch ::
    (m a -> (e -> m a) -> m a)          -- ^ @catch@ on the argument monad.
    -> ReaderT r m a                    -- ^ Computation to attempt.
    -> (e -> ReaderT r m a)             -- ^ Exception handler.
    -> ReaderT r m a
liftCatch f m h =
    ReaderT $ \ r -> f (runReaderT m r) (\ e -> runReaderT (h e) r)
