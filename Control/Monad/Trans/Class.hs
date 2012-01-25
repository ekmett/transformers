-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Classes for monad transformers.
--
-- A monad transformer makes new monad out of an existing monad, such
-- that computations of the old monad may be embedded in the new one.
-- To construct a monad with a desired set of features, one typically
-- starts with a base monad, such as @Identity@, @[]@ or 'IO', and
-- applies a sequence of monad transformers.
--
-- Most monad transformer modules include the special case of applying the
-- transformer to @Identity@.  For example, @State s@ is an abbreviation
-- for @StateT s Identity@.
--
-- Each monad transformer also comes with an operation @run@/XXX/ to
-- unwrap the transformer, exposing a computation of the inner monad.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Class (
    -- * Transformer class
    MonadTrans(..)

    -- * Examples
    -- ** Parsing
    -- $example1

    -- ** Parsing and counting
    -- $example2
  ) where

-- | The class of monad transformers.  Instances should satisfy the
-- following laws, which state that 'lift' is a transformer of monads:
--
-- * @'lift' . 'return' = 'return'@
--
-- * @'lift' (m >>= f) = 'lift' m >>= ('lift' . f)@

class MonadTrans t where
    -- | Lift a computation from the argument monad to the constructed monad.
    lift :: Monad m => m a -> t m a

{- $example1

One might define a parsing monad by adding a state (the 'String' remaining
to be parsed) to the @[]@ monad, which provides non-determinism:

> import Control.Monad.Trans.State
>
> type Parser = StateT String []

Then @Parser@ is an instance of @MonadPlus@: monadic sequencing implements
concatenation of parsers, while @mplus@ provides choice.
To use parsers, we need a primitive to run a constructed parser on an
input string:

> runParser :: Parser a -> String -> [a]
> runParser p s = [x | (x, "") <- runStateT p s]

Finally, we need a primitive parser that matches a single character,
from which arbitrarily complex parsers may be constructed:

> item :: Parser Char
> item = do
>     c:cs <- get
>     put cs
>     return c

In this example we use the operations @get@ and @put@ from
"Control.Monad.Trans.State", which are defined only for monads that are
applications of @StateT@.  Alternatively one could use monad classes
from other packages, which contain methods @get@ and @put@ with types
generalized over all suitable monads.
-}

{- $example2

We can define a parser that also counts by adding a @WriterT@ transformer:

> import Control.Monad.Trans.Class
> import Control.Monad.Trans.State
> import Control.Monad.Trans.Writer
> import Data.Monoid
>
> type Parser = WriterT (Sum Int) (StateT String [])

The function that applies a parser must now unwrap each of the monad
transformers in turn:

> runParser :: Parser a -> String -> [(a, Int)]
> runParser p s = [(x, n) | ((x, Sum n), "") <- runStateT (runWriterT p) s]

To define @item@ parser, we need to lift the @StateT@ operations through
the @WriterT@ transformers.

> item :: Parser Char
> item = do
>     c:cs <- lift get
>     lift (put cs)
>     return c

In this case, we were able to do this with 'lift', but operations with
more complex types require special lifting functions, which are provided
by monad transformers for which they can be implemented.  If you use
one of packages of monad classes, this lifting is handled automatically
by the instances of the classes, and you need only use the generalized
methods @get@ and @put@.

We can also define a primitive using the Writer:

> tick :: Parser ()
> tick = tell (Sum 1)

Then the parser will keep track of how many @tick@s it executes.
-}
