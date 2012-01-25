{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Error
-- Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de> 2001,
--                (c) Jeff Newbern 2003-2006,
--                (c) Andriy Palamarchuk 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- This monad transformer adds the ability to fail or throw exceptions
-- to a monad.
--
-- A sequence of actions succeeds, producing a value, only if all the
-- actions in the sequence are successful.  If one fails with an error,
-- the rest of the sequence is skipped and the composite action fails
-- with that error.
--
-- If the value of the error is not required, the variant in
-- "Control.Monad.Trans.Maybe" may be used instead.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Error (
    -- * The ErrorT monad transformer
    Error(..),
    ErrorList(..),
    ErrorT(..),
    mapErrorT,
    -- * Error operations
    throwError,
    catchError,
    -- * Lifting other operations
    liftCallCC,
    liftListen,
    liftPass,
    -- * Examples
    -- $examples
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Control.Applicative
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Instances ()
import System.IO

instance MonadPlus IO where
    mzero       = ioError (userError "mzero")
    m `mplus` n = m `catch` \_ -> n

-- | An exception to be thrown.
--
-- Minimal complete definition: 'noMsg' or 'strMsg'.
class Error a where
    -- | Creates an exception without a message.
    -- The default implementation is @'strMsg' \"\"@.
    noMsg  :: a
    -- | Creates an exception with a message.
    -- The default implementation of @'strMsg' s@ is 'noMsg'.
    strMsg :: String -> a

    noMsg    = strMsg ""
    strMsg _ = noMsg

instance Error IOException where
    strMsg = userError

-- | A string can be thrown as an error.
instance ErrorList a => Error [a] where
    strMsg = listMsg

-- | Workaround so that we can have a Haskell 98 instance @'Error' 'String'@.
class ErrorList a where
    listMsg :: String -> [a]

instance ErrorList Char where
    listMsg = id

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

#if !(MIN_VERSION_base(4,2,1))

-- These instances are in base-4.3

instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r

instance Monad (Either e) where
    return        = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r

instance MonadFix (Either e) where
    mfix f = let
        a = f $ case a of
            Right r -> r
            _       -> error "empty mfix argument"
        in a

#endif /* base to 4.2.0.x */

instance (Error e) => Alternative (Either e) where
    empty        = Left noMsg
    Left _ <|> n = n
    m      <|> _ = m

instance (Error e) => MonadPlus (Either e) where
    mzero            = Left noMsg
    Left _ `mplus` n = n
    m      `mplus` _ = m

-- | The error monad transformer. It can be used to add error handling
-- to other monads.
--
-- The @ErrorT@ Monad structure is parameterized over two things:
--
-- * e - The error type.
--
-- * m - The inner monad.
--
-- The 'return' function yields a successful computation, while @>>=@
-- sequences two subcomputations, failing on the first error.
newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

-- | Map the unwrapped computation using the given function.
--
-- * @'runErrorT' ('mapErrorT' f m) = f ('runErrorT' m@)
mapErrorT :: (m (Either e a) -> n (Either e' b))
          -> ErrorT e m a
          -> ErrorT e' n b
mapErrorT f m = ErrorT $ f (runErrorT m)

instance (Functor m) => Functor (ErrorT e m) where
    fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Functor m, Monad m) => Applicative (ErrorT e m) where
    pure a  = ErrorT $ return (Right a)
    f <*> v = ErrorT $ do
        mf <- runErrorT f
        case mf of
            Left  e -> return (Left e)
            Right k -> do
                mv <- runErrorT v
                case mv of
                    Left  e -> return (Left e)
                    Right x -> return (Right (k x))

instance (Functor m, Monad m, Error e) => Alternative (ErrorT e m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, Error e) => Monad (ErrorT e m) where
    return a = ErrorT $ return (Right a)
    m >>= k  = ErrorT $ do
        a <- runErrorT m
        case a of
            Left  l -> return (Left l)
            Right r -> runErrorT (k r)
    fail msg = ErrorT $ return (Left (strMsg msg))

instance (Monad m, Error e) => MonadPlus (ErrorT e m) where
    mzero       = ErrorT $ return (Left noMsg)
    m `mplus` n = ErrorT $ do
        a <- runErrorT m
        case a of
            Left  _ -> runErrorT n
            Right r -> return (Right r)

instance (MonadFix m, Error e) => MonadFix (ErrorT e m) where
    mfix f = ErrorT $ mfix $ \a -> runErrorT $ f $ case a of
        Right r -> r
        _       -> error "empty mfix argument"

instance (Error e) => MonadTrans (ErrorT e) where
    lift m = ErrorT $ do
        a <- m
        return (Right a)

instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
    liftIO = lift . liftIO

-- | Signal an error value @e@.
--
-- * @'runErrorT' ('throwErrorT' e) = 'return' ('Left' e)@
throwError :: (Monad m, Error e) => e -> ErrorT e m a
throwError l = ErrorT $ return (Left l)

-- | Handle an error.
catchError :: (Monad m, Error e) =>
    ErrorT e m a                -- ^ the inner computation
    -> (e -> ErrorT e m a)      -- ^ a handler for errors in the inner
                                -- computation
    -> ErrorT e m a
m `catchError` h = ErrorT $ do
    a <- runErrorT m
    case a of
        Left  l -> runErrorT (h l)
        Right r -> return (Right r)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (((Either e a -> m (Either e b)) -> m (Either e a)) ->
    m (Either e a)) -> ((a -> ErrorT e m b) -> ErrorT e m a) -> ErrorT e m a
liftCallCC callCC f = ErrorT $
    callCC $ \c ->
    runErrorT (f (\a -> ErrorT $ c (Right a)))

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m =>
    (m (Either e a) -> m (Either e a,w)) -> ErrorT e m a -> ErrorT e m (a,w)
liftListen listen = mapErrorT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m => (m (Either e a,w -> w) -> m (Either e a)) ->
    ErrorT e m (a,w -> w) -> ErrorT e m a
liftPass pass = mapErrorT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Left  l      -> (Left  l, id)
        Right (r, f) -> (Right r, f)

{- $examples

Wrapping an IO action that can throw an error @e@:

> type ErrorWithIO e a = ErrorT e IO a
> ==> ErrorT (IO (Either e a))

An IO monad wrapped in @StateT@ inside of @ErrorT@:

> type ErrorAndStateWithIO e s a = ErrorT e (StateT s IO) a
> ==> ErrorT (StateT s IO (Either e a))
> ==> ErrorT (StateT (s -> IO (Either e a,s)))

-}
