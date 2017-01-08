% Applied category theory and abstract algebra
% Gabriel Gonzalez
% January 8, 2017

# Function composition

You can chain pure functions using Haskell's function composition operator:

```haskell
(f . g) x = f (g x)
```

```haskell
>>> (not . even . length) [1, 2, 3]
True
>>> not (even (length [1, 2, 3]))
True
```

You can also define an identity function that does nothing::

```haskell
id x = x
```

```haskell
>>> id 4
4
```

# Composition laws

Composition and identity obey the following properties:

```haskell
id . f = f                 -- Left identity

f . id = f                 -- Right identity

(f . g) . h = f . (g . h)  -- Associativity
```

... which are true for all possible values of `f`, `g`, and `h`

# Unix philosophy

Function composition pipelines greatly resemble Unix command line pipelines:

Compare this:

```haskell
not . length . even
```

... to this:

```bash
$ ls /tmp | grep foo | wc -l
```

* Function composition (i.e. `.`) resembles the Unix pipe (i.e. `|`)

* Functions (`not`/`length`/`even`) resemble Unix commands (`ls`/`grep`/`wc`)

* You create useful programs by decomposing them into simple programs

* Each simple program "does one thing and does it well"

* We compose simple functions into function pipelines

**Goal**: We want to extend composability to more complex software architectures

**Punch line:** Category theory is about composability

# Overview

* Composable values
* Practical example
* Composable types
* Conclusion

# `Monoid`

The simplest "composable" interface in Haskell is the `Monoid` typeclass:

```haskell
class Monoid m where
    mempty :: m

    mappend :: m -> m -> m

(<>) :: Monoid m => m -> m -> m
x <> y = mappend x y
```

All `Monoid` implementations must obey the following `Monoid` laws:

```haskell
mempty <> x = x                -- Left identity

x <> mempty = x                -- Right identity

(x <> y) <> z = x <> (y <> z)  -- Associativity
```

Look familiar?

```haskell
id . f = f                     -- Left identity

f . id = f                     -- Right identity

(f . g) . h = f . (g . h)      -- Associativity
```

# Example `Monoid` instance

```haskell
instance Monoid [a] where
    mempty = []

    mappend x y = x ++ y
```

Lists obey the `Monoid` laws:

```haskell
[] ++ x = x                    -- mempty <> x = x

x ++ [] = x                    -- x <> mempty = x

(x ++ y) ++ z = x ++ (y ++ z)  -- (x <> y) <> z = x <> (y <> z)
```

# Example usage

```haskell
>>> [7] ++ [11]
[7, 11]
>>> [7] <> [11]
[7, 11]
>>> mempty :: [Integer]
[]
>>> "ABC" <> "DEF"  -- type String = [Char]
"ABCDEF"
>>> ['A', 'B', 'C'] <> ['D', 'E', 'F']
"ABCDEF"
```

# Unit

```haskell
instance Monoid () where
    mempty = ()

    mappend () () = ()
```

```haskell
>>> () <> ()
()
>>> mempty :: ()
()
```

# Question for the audience

What is the point of providing `Monoid` instances for lists or `()`?

Why not use `(++)` instead of `(<>)` and use `[]` instead of `mempty`?

# `Monoid`-generic functions

We can write functions that are generic over the `Monoid` interface

```haskell
mconcat :: Monoid m => [m] -> m
mconcat (x:xs) = mappend x (mconcat xs)
mconcat    []  = mempty
```

This `mconcat` function works for any type that implements `Monoid`

```haskell
>>> mconcat [[7, 11], [], [13]]
[7, 11, 13]
>>> mconcat [(), (), ()]
()
```

# `Monoid`-generic instances

We can also write `Monoid` instances generic over other `Monoid` instances

```haskell
instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)

    mappend (xL, yL) (xR, yR) = (mappend xL xR, mappend yL yR)
```

```haskell
>>> ([2], [3]) <> ([5], [7])
([2,5], [3,7])
>>> mempty :: ([Integer], [Integer])
([], [])
>>> mconcat [([], [2]), ([3, 5], [7]), ([], [11, 13])]
([3,5],[2,7,11,13])
```

# `Monoid`-generic proofs

We can even write proofs that are generic over the `Monoid` laws

```haskell
(xL, xR) <> mempty

-- x <> y = mappend x y
= mappend (xL, xR) mempty

-- mempty = (mempty, mempty)
= mappend (xL, xR) (mempty, mempty)

-- mappend (xL, yL) (xR, yR) = (mappend xL xR, mappend yL yR)
= (mappend xL mempty, mappend xR mempty)

-- mappend x mempty = x
= (xL, xR)
```

# Nesting `Monoid`s

The `Monoid` instance for 2-tuples works for any type `a` and `b`

... even if `a` and `b` are tuples!

That means that this type is a `Monoid`:

```haskell
(([Integer], [Integer]), [Integer])
```

* `[Integer]` is a `Monoid`
* ... and `(a, b)` is a `Monoid` if `a` and `b` are `Monoid`s ...
* ... therefore `([Integer], [Integer])` is a `Monoid`
* ... therefore `(([Integer], [Integer]), [Integer])` is a `Monoid`

We can nest tuples of lists indefinitely and they still form a `Monoid`:

```haskell
>>> ([1],([2],([3],([4],([5],()))))) <> ([6],([7],([8],([9],([10],())))))
([1,6],([2,7],([3,8],([4,9],([5,10],())))))
```

# `IO`

GHC 8 provides the following `Monoid` instance for `IO`:

```haskell
instance Monoid b => Monoid (IO b) where
    mempty = return mempty

    mappend ioA ioB = do
        a <- ioA
        b <- ioB
        return (mappend a b)
```

This means that these types are legal `Monoid`s:

```haskell
>>> getLine :: IO String
Hello<Enter>
"Hello"
>>> getLine <> getLine :: IO String
Hello<Enter>
, world!<Enter>
"Hello, world!"
```

```haskell
>>> print 1 :: IO ()
1
>>> print 1 <> print 2 :: IO ()
1
2
```

# Equational reasoning example

```haskell
getLine <> getLine

-- x <> y = mappend x y
= mappend getLine getLine

-- mappend ioA ioB = do a <- ioA; b <- ioB; return (mappend a b)
= do a <- getLine; b <- getLine; return (mappend a b)

-- mappend x y = x ++ y :: String
= do a <- getLine; b <- getLine; return (a ++ b)
```

```haskell
print 1 <> print 2

-- x <> y = mappend x y
= mappend (print 1) (print 2)

-- mappend ioA ioB = do a <- ioA; b <- ioB; return (mappend a b)
= do () <- print 1; () <- print 2; return (mappend () ())

-- mappend () () = ()
= do print 1; print 2; return ()
```

# Example

```haskell
import Data.Monoid

promptName :: IO (IO ())
promptName = do
    putStrLn "Enter your name:"
    name <- getLine
    return $ do
        putStrLn ("Your name is: " ++ name)

promptAge :: IO (IO ())
promptAge = do
    putStrLn "Enter your age:"
    age <- readLn
    return $ do
        putStrLn ("Your age is: " ++ show (age :: Integer))

main :: IO ()
main = do
    respond <- promptName <> promptAge
    respond
```

```
>>> main
Enter your name:
Gabriel Gonzalez
Enter your age:
31
Your name is: Gabriel Gonzalez
Your age is: 31
```

# Functions

Here's another `Monoid` instance:

```haskell
instance Monoid b => Monoid (a -> b) where
    mempty = \_ -> mempty

    mappend f g = \x -> mappend (f x) (g x)
```

```haskell
>>> putStrLn "Hi"
Hi
>>> (putStrLn <> putStrLn) "Hi"
Hi
Hi
```

# Chaining different monoids

The type of `putStrLn` implements `Monoid`:

```haskell
putStrLn :: String -> IO ()
```

... thanks to the following `Monoid` instances:

```haskell
instance Monoid ()
instance Monoid b => Monoid (IO b)
instance Monoid b => Monoid (a -> b)
```

```haskell
mappend putStrLn putStrLn

-- mappend f g = \x -> mappend (f x) (g x)
= \x -> mappend (putStrLn x) (putStrLn x)

-- mappend ioA ioB = do a <- ioA; b <- ioB; return (mappend a b)
= \x -> do () <- putStrLn x; () <- putStrLn x; return (mappend () ())

-- mappend () () = ()
= \x -> do putStrLn x; putStrLn x; return ()
```

# Questions?

* Composable values
* Practical example
* Composable types
* Conclusion

# New `Monoid`: `Transaction`

Let's add a new `Monoid` to our toolbox:

```haskell
newtype Transaction a = Transaction { getTransaction :: STM a }

instance Monoid (Transaction a) where
    mempty = Transaction retry

    mappend (Transaction l) (Transaction r) = Transaction (l `orElse` r)
```

A `Transaction` can use `mempty` to block if they are not ready to complete

`mappend` takes the first transaction that does not block

# New `Monoid`: `Managed`

```haskell
-- | A managed resource
newtype Managed a = Managed { (>>-) :: forall r . (a -> IO r) -> IO r }

instance Monoid a => Monoid (Managed a) where ...
```

`mempty` acquires an empty resource

`mappend` acquires two resources and combines them

# Event streams are `Monoid`s

Let's layer `Managed` on top of `Transaction` to create a new `Monoid`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM (STM)
import Control.Monad (forever)
import Control.Monad.Managed (Managed, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import qualified Control.Monad.Managed          as Managed
import qualified Network.HTTP.Types             as HTTP
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified System.IO                      as IO

newtype Transaction a = Transaction { getTransaction :: STM a }

instance Monoid (Transaction a) where
    mempty = Transaction STM.retry

    mappend (Transaction l) (Transaction r) = Transaction (l `STM.orElse` r)

data Event = Tick | KeyPress Char | Message ByteString deriving (Show)

chars :: Managed (Transaction Event)
chars = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let thread = forever (do
            char <- getChar
            STM.atomically (TBQueue.writeTBQueue queue (KeyPress char)) )
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

ticks :: Managed (Transaction Event)
ticks = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let thread = forever (do
            Concurrent.threadDelay 1000000
            STM.atomically (TBQueue.writeTBQueue queue Tick) )
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

messages :: Managed (Transaction Event)
messages = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let application request respond = do
            bytestring <- Wai.strictRequestBody request
            STM.atomically (TBQueue.writeTBQueue queue (Message bytestring))
            respond (Wai.responseBuilder HTTP.status200 [] "")
    let thread = Warp.run 8080 application
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

events :: Managed (Transaction Event)
events = chars <> ticks <> messages

main :: IO ()
main = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho      IO.stdin False
    Managed.runManaged (do
        transaction <- events
        liftIO (forever (do
            event <- STM.atomically (getTransaction transaction)
            print event ) ))
```

# Configurable event streams are `Monoid`s

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM (STM)
import Control.Monad (forever)
import Control.Monad.Managed (Managed, liftIO)
import Data.Binary.Builder (Builder)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import qualified Control.Monad.Managed          as Managed
import qualified Network.HTTP.Types             as HTTP
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified System.IO                      as IO

newtype Transaction a = Transaction { getTransaction :: STM a }

instance Monoid (Transaction a) where
    mempty = Transaction STM.retry

    mappend (Transaction l) (Transaction r) = Transaction (l `STM.orElse` r)

data Event = Tick | KeyPress Char | Message ByteString deriving (Show)

data Config = Config { delay :: Int, response :: Builder }

chars :: Config -> Managed (Transaction Event)
chars _ = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let thread = forever (do
            char <- getChar
            STM.atomically (TBQueue.writeTBQueue queue (KeyPress char)) )
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

ticks :: Config -> Managed (Transaction Event)
ticks config = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let thread = forever (do
            Concurrent.threadDelay (delay config)
            STM.atomically (TBQueue.writeTBQueue queue Tick) )
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

messages :: Config -> Managed (Transaction Event)
messages config = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let application request respond = do
            bytestring <- Wai.strictRequestBody request
            STM.atomically (TBQueue.writeTBQueue queue (Message bytestring))
            respond (Wai.responseBuilder HTTP.status200 [] (response config))
    let thread = Warp.run 8080 application
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

events :: Config -> Managed (Transaction Event)
events = chars <> ticks <> messages

main :: IO ()
main = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho      IO.stdin False
    Managed.runManaged (do
        transaction <- events (Config 2000000 "Thanks!\n")
        liftIO (forever (do
            event <- STM.atomically (getTransaction transaction)
            print event ) ))
```

# Named and configurable event streams are `Monoid`s

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM (STM)
import Control.Monad (forever)
import Control.Monad.Managed (Managed, liftIO)
import Data.Binary.Builder (Builder)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import qualified Control.Monad.Managed          as Managed
import qualified Network.HTTP.Types             as HTTP
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified System.IO                      as IO

newtype Transaction a = Transaction { getTransaction :: STM a }

instance Monoid (Transaction a) where
    mempty = Transaction STM.retry

    mappend (Transaction l) (Transaction r) = Transaction (l `STM.orElse` r)

data Event = Tick | KeyPress Char | Message ByteString deriving (Show)

data Config = Config { delay :: Int, response :: Builder }

chars :: ([String], Config -> Managed (Transaction Event))
chars = (["Characters"], handler)
  where
    handler _ = do
        queue <- liftIO (TBQueue.newTBQueueIO 100)
        let thread = forever (do
                char <- getChar
                STM.atomically (TBQueue.writeTBQueue queue (KeyPress char)) )
        _ <- Managed.managed (Async.withAsync thread)
        return (Transaction (TBQueue.readTBQueue queue))

ticks :: ([String], Config -> Managed (Transaction Event))
ticks = (["Ticks"], handler)
  where
    handler config = do
        queue <- liftIO (TBQueue.newTBQueueIO 100)
        let thread = forever (do
                Concurrent.threadDelay (delay config)
                STM.atomically (TBQueue.writeTBQueue queue Tick) )
        _ <- Managed.managed (Async.withAsync thread)
        return (Transaction (TBQueue.readTBQueue queue))

messages :: ([String], Config -> Managed (Transaction Event))
messages = (["Messages"], handler)
  where
    handler config = do
        queue <- liftIO (TBQueue.newTBQueueIO 100)
        let application request respond = do
                bytestring <- Wai.strictRequestBody request
                STM.atomically (TBQueue.writeTBQueue queue (Message bytestring))
                respond (Wai.responseBuilder HTTP.status200 [] (response config))
        let thread = Warp.run 8080 application
        _ <- Managed.managed (Async.withAsync thread)
        return (Transaction (TBQueue.readTBQueue queue))

events :: ([String], Config -> Managed (Transaction Event))
events = chars <> ticks <> messages

main :: IO ()
main = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho      IO.stdin False
    Managed.runManaged (do
        let (plugins, makeTransaction) = events
        transaction <- makeTransaction (Config 2000000 "Thanks!\n")
        liftIO (putStrLn "Plugins enabled:")
        let printPlugin plugin = liftIO (putStrLn ("* " ++ plugin))
        mconcat (map printPlugin plugins)
        liftIO (forever (do
            event <- STM.atomically (getTransaction transaction)
            print event ) ))
```

# Questions?

* Composable values
* Practical example
* Composable types
* Conclusion

# Shared patterns

Let's revisit the `Monoid` instance for `IO`:

```haskell
instance Monoid b => Monoid (IO b) where
    mempty = return mempty

    mappend mA mB = do
        a <- mA
        b <- mB
        return (mappend a b)
```

Notice how there's nothing really `IO`-specific about this implementation

We only use `return` and `do`-notation, which work for any `Monad`

Therefore: we can write the exact same `Monoid` instance for any `Monad`

... and Haskell has lots of `Monad`s (in case you didn't notice)

# Functions

Our original `Monoid` instance for functions could have been written this way:

```haskell
instance Monoid b => Monoid (a -> b) where
    mempty = return mempty

    mappend mA mB = do
        a <- mA
        b <- mB
        return (mappend a b)
```

This works because `(a -> b)` is syntactic sugar for `(->) a b`

... and there is a `Monad` instance for `(->) a`:

```haskell
instance Monad ((->) a) where ...
```

# Pairs

Our original `Monoid` instance for pairs could have been written this way:

```haskell
instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = return mempty

    mappend mA mB = do
        a <- mA
        b <- mB
        return (mappend a b)
```

This works because `(a, b)` is syntactic sugar for `(,) a b`

... and there is a `Monad` instance for `(,) a`:

```haskell
instance Monoid a => Monad ((,) a) where ...
```

# The pattern

In theory, we could write the following very general instance:

```haskell
{-# LANGUAGE FlexibleInstances #-}

instance (Monad f, Monoid m) => Monoid (f m) where
    mempty = return mempty

    mappend mA mB = do
        a <- pA
        b <- pB
        return (mappend a b)
```

This instance always obeys the `Monoid` laws no matter what `m` and `b` are

In practice, we can't do this because we'd get overlapping instances 😕

# `Applicative`

In fact, we can generalize this to any type that implements `Applicative`:

```haskell
instance (Applicative f, Monoid m) => Monoid (f m) where
    mempty = pure mempty

    mappend = liftA2 mappend
```

Every `Applicative` implements `pure` and (indirectly) `liftA2`:

```haskell
pure :: Applicative f => a -> f a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
```

```haskell
mempty  :: Monoid m => m

mappend :: Monoid m => m -> m -> m

pure mempty    :: (Monoid m, Applicative f) => f m

liftA2 mappend :: (Monoid m, Applicative f) => f m -> f m -> f m
```

# Chaining `Applicative`s

We can keep chaining `pure` and `liftA2`

```haskell
pure (pure (pure (... (pure mempty) ...)))
    :: ( Applicative f1
       , Applicative f2
       , Applicative f3
       , ...
       , Applicative fn
       , Monoid m
       )
    => f1 (f2 (f3 (... (fn m) ...)))

liftA2 (liftA2 (liftA2 ... (liftA2 mappend) ...))
    :: ( Applicative f1
       , Applicative f2
       , Applicative f3
       , ...
       , Applicative fn
       , Monoid m
       )
    => f1 (f2 (f3 (... (fn m) ...)))
    -> f1 (f2 (f3 (... (fn m) ...)))
    -> f1 (f2 (f3 (... (fn m) ...)))
```

# Example chained `Monoid` instance

```haskell
newtype Plugin = Plugin ([String], Config -> Managed (Transaction Event))

instance Monoid Plugin where
    mempty = Plugin (pure (pure (pure mempty)))

    mappend (Plugin x) (Plugin y) =
        Plugin (liftA2 (liftA2 (liftA2 mappend)) x y)
```

We don't need to prove the `Monoid` laws for `Plugin`

`Applicative` laws guarantee that this implementation is correct by construction

(Proof omitted)

# `Applicative` pipelines

A type like this:

```haskell
( Applicative f1
, Applicative f2
, Applicative f3
, ...
, Applicative fn
, Monoid m
) => f1 (f2 (f3 (... (fn m) ...)))
```

... is "an `Applicative` pipeline", analogous to a Unix pipeline:

* Each stage is an `Applicative` that "does one thing and does it well"
* We connect stages together with a "universal interface": `Monoid`
* The input to each stage is a `Monoid` and the output of each stage is a `Monoid`
* We can mix and match `Applicatives` just like we mix and match Unix commands

There's no limit to how complex of a pipeline we can build

# `Applicative` composition

We can define a type to "compose" `Applicative`s:

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative

newtype (f `O` g) a = Compose { unCompose :: f (g a) } deriving (Functor)

instance (Applicative f, Applicative g) => Applicative (f `O` g) where
    pure x = Compose (pure (pure x))

    Compose l <*> Compose r = Compose (liftA2 (<*>) l r)

instance (Monoid a, Applicative f, Applicative g) => Monoid ((f `O` g) a) where
    mempty = pure mempty

    mappend = liftA2 mappend
```

# `Applicative` identity

We can also define an `Identity` of this type-level composition operator:

```
newtype Identity a = Identity { runIdentity :: a }

instance Applicative Identity where
    pure a = Identity a

    Identity f <*> Identity x = Identity (f x)

instance Monoid a => Monoid (Identity a) where
    mempty = pure mempty

    mappend = liftA2 mappend
```

# `Applicative` composition laws

We can even show that `Applicative` composition and identity obey these laws:

```haskell
Identity `O` f ≅ f                 -- Left identity

f `O` Identity ≅ f                 -- Right identity

(f `O` g) `O` h ≅ f `O` (g `O` h)  -- Associativity
```

Look familiar?

```haskell
id . f = f                         -- Left identity

f . id = f                         -- Right identity

(f . g) . h = f . (g . h)          -- Associativity
```

# Example `Applicative` composition

This means that we could also write `Plugin` as:

```haskell
type Plugin = ((,) [String] `O` (->) Config `O` Managed) (Transaction Event)
```

... and it's automatically a `Monoid`!

We are literally composing an `Applicative` pipeline

# Conclusion

We began from simple composable functions:

* Composition operator: `(.)`
* Identity: `id`
* Composable units: functions

... then generalized that to other composable values using `Monoid`s:

* Composition operator: `(<>)`
* Identity: `mempty`
* Composable units: `Monoid` values

... then generalized that further to composable types using `Applicative`s:

* Composition operator: `O`
* Identity: `Identity`
* Composable units: `Applicative` types

Abstractions from category theory (and abstract algebra) help decompose programs
into simple composable building blocks

# Questions?

You can find these slides at:

[https://github.com/Gabriel439/slides/tree/master/lambdaconf/category.md](https://github.com/Gabriel439/slides/tree/master/lambdaconf/category.md)

You can also reach me at:

* Email - [Gabriel439@gmail.com](mailto:Gabriel439@gmail.com)
* Twitter - [GabrielG439](https://twitter.com/GabrielG439)
