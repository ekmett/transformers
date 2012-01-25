-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Writer.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The strict 'WriterT' monad transformer, which adds collection of
-- outputs (such as a count or string output) to a given monad.
--
-- This version builds its output strictly; for a lazy version, see
-- "Control.Monad.Trans.Writer.Lazy", which has the same interface.
--
-- This monad transformer provides only limited access to the output
-- during the computation.  For more general access, use
-- "Control.Monad.Trans.State" instead.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Writer.Strict (
    -- * The Writer monad
    Writer,
    writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT monad transformer
    WriterT(..),
    execWriterT,
    mapWriterT,
    -- * Writer operations
    tell,
    listen,
    listens,
    pass,
    censor,
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
import Data.Monoid

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by the type @w@ of output to accumulate.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
type Writer w = WriterT w Identity

-- | Construct a writer computation from a (result, output) pair.
-- (The inverse of 'runWriter'.)
writer :: (a, w) -> Writer w a
writer = WriterT . Identity

-- | Unwrap a writer computation as a (result, output) pair.
-- (The inverse of 'writer'.)
runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

-- | Extract the output from a writer computation.
--
-- * @'execWriter' m = 'snd' ('runWriter' m)@
execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriter' ('mapWriter' f m) = f ('runWriter' m@)
mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

-- | Extract the output from a writer computation.
--
-- * @'execWriterT' m = 'liftM' 'snd' ('runWriterT' m)@
execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = do
    (_, w) <- runWriterT m
    return w

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriterT' ('mapWriterT' f m) = f ('runWriterT' m@)
mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)

instance (Functor m) => Functor (WriterT w m) where
    fmap f = mapWriterT $ fmap $ \ (a, w) -> (f a, w)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure a  = WriterT $ pure (a, mempty)
    f <*> v = WriterT $ liftA2 k (runWriterT f) (runWriterT v)
      where k (a, w) (b, w') = (a b, w `mappend` w')

instance (Monoid w, Alternative m) => Alternative (WriterT w m) where
    empty   = WriterT empty
    m <|> n = WriterT $ runWriterT m <|> runWriterT n

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= k  = WriterT $ do
        (a, w)  <- runWriterT m
        (b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
    fail msg = WriterT $ fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero       = WriterT mzero
    m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
    mfix m = WriterT $ mfix $ \ ~(a, _) -> runWriterT (m a)

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        a <- m
        return (a, mempty)

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell w = WriterT $ return ((), w)

-- | @'listen' m@ is an action that executes the action @m@ and adds its
-- output to the value of the computation.
--
-- * @'runWriterT' ('listen' m) = 'liftM' (\\(a, w) -> ((a, w), w)) ('runWriterT' m)@
listen :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
    (a, w) <- runWriterT m
    return ((a, w), w)

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
--
-- * @'runWriterT' ('listens' f m) = 'liftM' (\\(a, w) -> ((a, f w), w)) ('runWriterT' m)@
listens :: (Monoid w, Monad m) => (w -> b) -> WriterT w m a -> WriterT w m (a, b)
listens f m = do
    (a, w) <- listen m
    return (a, f w)

-- | @'pass' m@ is an action that executes the action @m@, which returns
-- a value and a function, and returns the value, applying the function
-- to the output.
--
-- * @'runWriterT' ('pass' m) = 'liftM' (\\((a, f), w) -> (a, f w)) ('runWriterT' m)@
pass :: (Monoid w, Monad m) => WriterT w m (a, w -> w) -> WriterT w m a
pass m = WriterT $ do
    ((a, f), w) <- runWriterT m
    return (a, f w)

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\x -> (x,f)) m)@
--
-- * @'runWriterT' ('censor' f m) = 'liftM' (\\(a, w) -> (a, f w)) ('runWriterT' m)@
censor :: (Monoid w, Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = pass $ do
    a <- m
    return (a, f)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (Monoid w) => ((((a,w) -> m (b,w)) -> m (a,w)) -> m (a,w)) ->
    ((a -> WriterT w m b) -> WriterT w m a) -> WriterT w m a
liftCallCC callCC f = WriterT $
    callCC $ \c ->
    runWriterT (f (\a -> WriterT $ c (a, mempty)))

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m (a,w) -> (e -> m (a,w)) -> m (a,w)) ->
    WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a
liftCatch catchError m h =
    WriterT $ runWriterT m `catchError` \e -> runWriterT (h e)
