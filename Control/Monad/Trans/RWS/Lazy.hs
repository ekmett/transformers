-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.RWS.Lazy
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- A monad transformer that combines 'ReaderT', 'WriterT' and 'StateT'.
-- This version is lazy; for a strict version, see
-- "Control.Monad.Trans.RWS.Strict", which has the same interface.
-----------------------------------------------------------------------------

module Control.Monad.Trans.RWS.Lazy (
    -- * The RWS monad
    RWS,
    rws,
    runRWS,
    evalRWS,
    execRWS,
    mapRWS,
    withRWS,
    -- * The RWST monad transformer
    RWST(..),
    evalRWST,
    execRWST,
    mapRWST,
    withRWST,
    -- * Reader operations
    ask,
    local,
    asks,
    -- * Writer operations
    tell,
    listen,
    listens,
    pass,
    censor,
    -- * State operations
    get,
    put,
    modify,
    gets,
    -- * Lifting other operations
    liftCallCC,
    liftCallCC',
    liftCatch,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Monoid

-- | A monad containing an environment of type @r@, output of type @w@
-- and an updatable state of type @s@.
type RWS r w s = RWST r w s Identity

-- | Construct an RWS computation from a function.
-- (The inverse of 'runRWS'.)
rws :: (r -> s -> (a, s, w)) -> RWS r w s a
rws f = RWST (\ r s -> Identity (f r s))

-- | Unwrap an RWS computation as a function.
-- (The inverse of 'rws'.)
runRWS :: RWS r w s a -> r -> s -> (a, s, w)
runRWS m r s = runIdentity (runRWST m r s)

evalRWS :: RWS r w s a -> r -> s -> (a, w)
evalRWS m r s = let
    (a, _, w) = runRWS m r s
    in (a, w)

execRWS :: RWS r w s a -> r -> s -> (s, w)
execRWS m r s = let
    (_, s', w) = runRWS m r s
    in (s', w)

mapRWS :: ((a, s, w) -> (b, s, w')) -> RWS r w s a -> RWS r w' s b
mapRWS f = mapRWST (Identity . f . runIdentity)

withRWS :: (r' -> s -> (r, s)) -> RWS r w s a -> RWS r' w s a
withRWS = withRWST

-- ---------------------------------------------------------------------------
-- | A monad transformer adding reading an environment of type @r@,
-- collecting an output of type @w@ and updating a state of type @s@
-- to an inner monad @m@.
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

evalRWST :: (Monad m) => RWST r w s m a -> r -> s -> m (a, w)
evalRWST m r s = do
    ~(a, _, w) <- runRWST m r s
    return (a, w)

execRWST :: (Monad m) => RWST r w s m a -> r -> s -> m (s, w)
execRWST m r s = do
    ~(_, s', w) <- runRWST m r s
    return (s', w)

mapRWST :: (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST f m = RWST $ \r s -> f (runRWST m r s)

withRWST :: (r' -> s -> (r, s)) -> RWST r w s m a -> RWST r' w s m a
withRWST f m = RWST $ \r s -> uncurry (runRWST m) (f r s)

instance (Functor m) => Functor (RWST r w s m) where
    fmap f m = RWST $ \r s ->
        fmap (\ ~(a, s', w) -> (f a, s', w)) $ runRWST m r s

instance (Monoid w, Functor m, Monad m) => Applicative (RWST r w s m) where
    pure = return
    (<*>) = ap

instance (Monoid w, Functor m, MonadPlus m) => Alternative (RWST r w s m) where
    empty = mzero
    (<|>) = mplus

instance (Monoid w, Monad m) => Monad (RWST r w s m) where
    return a = RWST $ \_ s -> return (a, s, mempty)
    m >>= k  = RWST $ \r s -> do
        ~(a, s', w)  <- runRWST m r s
        ~(b, s'',w') <- runRWST (k a) r s'
        return (b, s'', w `mappend` w')
    fail msg = RWST $ \_ _ -> fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (RWST r w s m) where
    mzero       = RWST $ \_ _ -> mzero
    m `mplus` n = RWST $ \r s -> runRWST m r s `mplus` runRWST n r s

instance (Monoid w, MonadFix m) => MonadFix (RWST r w s m) where
    mfix f = RWST $ \r s -> mfix $ \ ~(a, _, _) -> runRWST (f a) r s

instance (Monoid w) => MonadTrans (RWST r w s) where
    lift m = RWST $ \_ s -> do
        a <- m
        return (a, s, mempty)

instance (Monoid w, MonadIO m) => MonadIO (RWST r w s m) where
    liftIO = lift . liftIO

-- ---------------------------------------------------------------------------
-- Reader operations

-- | Fetch the value of the environment.
ask :: (Monoid w, Monad m) => RWST r w s m r
ask = RWST $ \r s -> return (r, s, mempty)

-- | Execute a computation in a modified environment
local :: (Monoid w, Monad m) => (r -> r) -> RWST r w s m a -> RWST r w s m a
local f m = RWST $ \r s -> runRWST m (f r) s

-- | Retrieve a function of the current environment.
asks :: (Monoid w, Monad m) => (r -> a) -> RWST r w s m a
asks f = do
    r <- ask
    return (f r)

-- ---------------------------------------------------------------------------
-- Writer operations

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monoid w, Monad m) => w -> RWST r w s m ()
tell w = RWST $ \_ s -> return ((),s,w)

-- | @'listen' m@ is an action that executes the action @m@ and adds its
-- output to the value of the computation.
listen :: (Monoid w, Monad m) => RWST r w s m a -> RWST r w s m (a, w)
listen m = RWST $ \r s -> do
    ~(a, s', w) <- runRWST m r s
    return ((a, w), s', w)

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
listens :: (Monoid w, Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

-- | @'pass' m@ is an action that executes the action @m@, which returns
-- a value and a function, and returns the value, applying the function
-- to the output.
pass :: (Monoid w, Monad m) => RWST r w s m (a, w -> w) -> RWST r w s m a
pass m = RWST $ \r s -> do
    ~((a, f), s', w) <- runRWST m r s
    return (a, s', f w)

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\x -> (x,f)) m)@
censor :: (Monoid w, Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
censor f m = pass $ do
    a <- m
    return (a, f)

-- ---------------------------------------------------------------------------
-- State operations

-- | Fetch the current value of the state within the monad.
get :: (Monoid w, Monad m) => RWST r w s m s
get = RWST $ \_ s -> return (s, s, mempty)

-- | @'put' s@ sets the state within the monad to @s@.
put :: (Monoid w, Monad m) => s -> RWST r w s m ()
put s = RWST $ \_ _ -> return ((), s, mempty)

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
modify :: (Monoid w, Monad m) => (s -> s) -> RWST r w s m ()
modify f = do
    s <- get
    put (f s)
 
-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: (Monoid w, Monad m) => (s -> a) -> RWST r w s m a
gets f = do
    s <- get
    return (f s)

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original state on entering the
-- continuation.
liftCallCC :: (Monoid w) =>
    ((((a,s,w) -> m (b,s,w)) -> m (a,s,w)) -> m (a,s,w)) ->
    ((a -> RWST r w s m b) -> RWST r w s m a) -> RWST r w s m a
liftCallCC callCC f = RWST $ \r s ->
    callCC $ \c ->
    runRWST (f (\a -> RWST $ \_ _ -> c (a, s, mempty))) r s

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current state on entering the continuation.
liftCallCC' :: (Monoid w) =>
    ((((a,s,w) -> m (b,s,w)) -> m (a,s,w)) -> m (a,s,w)) ->
    ((a -> RWST r w s m b) -> RWST r w s m a) -> RWST r w s m a
liftCallCC' callCC f = RWST $ \r s ->
    callCC $ \c ->
    runRWST (f (\a -> RWST $ \_ s' -> c (a, s', mempty))) r s

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m (a,s,w) -> (e -> m (a,s,w)) -> m (a,s,w)) ->
    RWST l w s m a -> (e -> RWST l w s m a) -> RWST l w s m a
liftCatch catchError m h =
    RWST $ \r s -> runRWST m r s `catchError` \e -> runRWST (h e) r s
