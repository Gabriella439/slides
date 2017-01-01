% Applied category theory and abstract algebra
% Gabriel Gonzalez
% January ??, 2017

# Equational reasoning

Purely functional languages promote "equational reasoning"

Equational reasoning means:

* You specify program behavior in terms of equations
* You reason about code by substituting equals for equals

# Equational reasoning example

```haskell
twice x = x >> x  -- Same as: twice x = do x; x

main = twice (twice (print 1))
```

We can reason about what `main` does by substituting equals for equals:

```haskell
main = twice (twice (print 1))

-- twice x = x >> x
main = twice (print 1 >> print 1)

-- twice x = x >> x
main = (print 1 >> print 1) >> (print 1 >> print 1)
```

# Question for the audience

Is this `main`:

```haskell
main = (print 1 >> print 1) >> (print 1 >> print 1)
```

... equal to this `main`:

```haskell
main = print 1 >> print 1 >> print 1 >> print 1
```

# Proving properties

We can reason about more than just `main`

We can also prove "properties" about functions we define

For example, consider the following definitions of `(.)` and `id`:

```haskell
(f . g) x = f (g x)

id x = x
```

We can prove properties like:

```haskell
f . id = f

id . f = f

(f . g) . h = f . (g . h)
```

... which are true for all possible values of `f`, `g`, and `h`

# Example proof

```haskell
f . id

-- Extensionality: k = \x -> k x
= \x -> (f . id) x

-- (f . g) x = f (g x)
= \x -> f (id x)

-- id x = x
= \x -> f x

-- Extensionality: k = \x -> k x
= f
```

Caveat: extensionality is not always valid in Haskell 

This talk will assume a hypothetical wart-free and total subset of Haskell

# Scaling proofs

These proofs can become time-consuming as code grows more complex

We would like to prove high-level properties with ease as our projects grow

We can prove complex things by decomposing them into simpler, composable proofs

This is equivalent to approaching proofs with the Unix philosophy

**Punch line:** Proofs + Unix Philosophy = Category theory (and Abstract algebra)

# Goals

Proofs are not the goal; proofs are a means to an end

Fluency in proofs translates into the following goals:

* improved code reasoning skills
* faster prototyping
* simpler and more coherent APIs
* fewer corner cases
* ease of code maintenance

# Unix philosophy

The Unix philosophy states that programs should:

* do one thing and do it well, by ...
* ... working together ...
* ... using universal interfaces (such as streams of text)

This worked really well for command line tools, but never grew out of that niche

Why?

# Universal interfaces

A stream of text is not a universal interface

However, mathematicians have spent decades studying reusable interfaces

We can expand the Unix philosophy to new domains using mathematical interfaces

# Overview

# `Monoid`

The simplest reusable interface in Haskell is the `Monoid` typeclass:

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
>>> ([7, 11] <> []) <> [13]
[7, 11, 13]
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
>>> mconcat [] :: [Integer]
[]
>>> mconcat [(), (), ()]
()
>>> mconcat [] :: ()
()
```

# `Monoid`-generic instances

We can write `Monoid` instances that are generic over other `Monoid` instances

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

We can write proofs that are generic over the `Monoid` laws

```haskell
(a, b) <> mempty

-- x <> y = mappend x y
= mappend (a, b) mempty

-- mempty = (mempty, mempty)
= mappend (a, b) (mempty, mempty)

-- mappend (xL, yL) (xR, yR) = (mappend xL xR, mappend yL yR)
= (mappend a mempty, mappend b mempty)

-- mappend x mempty = x
= (a, b)
```

# Question for the audience

What is the result of this:

```haskell
>>> (([2], [3]), [5]) <> (([7], [11]), [13])
???
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
>>> getLine <> getLine :: IO String
Hello, <Enter>
world!<Enter>
"Hello, world!"
```

```haskell
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

# Question for the audience

What does this program do?

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

# Functions

Here's another `Monoid` instance:

```haskell
instance Monoid b => Monoid (a -> b) where
    mempty = \_ -> mempty

    mappend f g = \x -> mappend (f x) (g x)
```

What do you think this does?

```haskell
>>> (putStrLn <> putStrLn) "Hi"
???
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

This works because there is a `Monad` instance for functions:

```haskell
instance Monad ((->) a) where
    ...
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

This works because there is a `Monad` instance for pairs:

```haskell
instance Monoid a => Monad ((,) a) where
    ...
```

# The pattern

In theory, we could write the following very general instance:

```haskell
{-# LANGUAGE FlexibleInstances #-}

instance (Monad m, Monoid b) => Monoid (m b) where
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
newtype Example = Example (F1 (F2 (F3 (... (FN M) ...))))

instance Example where
    mempty = Example (pure (pure (pure (... (pure mempty) ...))))

    mappend (Example x) (Example y) =
        (Example (liftA2 (liftA2 (liftA2 (... (liftA2 mappend) ...))) x y)
```

We don't need to prove the `Monoid` laws for `Example`

`Applicative` laws guarantee that this derived `Monoid` is correct by construction

(Proof omitted)

# `Applicative` pipelines

This implies that we can automatically derive a `Monoid` instance for any type
of the form:

```haskell
( Applicative f1
, Applicative f2
, Applicative f3
, ...
, Applicative fn
, Monoid m
) => f1 (f2 (f3 (... (fn m) ...)))
```

Think of this as "an `Applicative` pipeline", analogous to a Unix pipeline:

* Each stage is an `Applicative` that "does one thing and does it well"
* We connect stages together with a "universal interface": `Monoid`
* The input to each stage is a `Monoid` and the output of each stage is a `Monoid`
* We can mix and match `Applicatives` just like we mix and match Unix commands

There's no limit to how complex of a pipeline we can build

# Questions?

# Toolbox

Let's expand our toolbox of `Monoid`s and `Applicative`s that we can chain

We'll add a new `Monoid`: `STM`

We'll add a new `Applicative`: `Managed`

# `STM` - software transactional memory

```haskell
data STM a

instance Monad       STM
instance Alternative STM

newTVarIO :: a -> IO (TVar a)

readTVar :: TVar a -> STM a

writeTVar :: TVar a -> a -> STM ()

atomically :: STM a -> IO a
```

```haskell
example :: IO ()
example = do
    tvar <- newTVarIO 0

    let transaction :: STM ()
        transaction = do
            x <- readTVar tvar
            writeTVar tvar (x + 1) )

    atomically transaction
```

# Example use of `STM`

```haskell
import Control.Applicative ((<|>), empty)
import Control.Concurrent (forkIO)
import Control.Monad (replicateM_)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)

main = do
    balanceRef <- newTVarIO (0 :: Integer)

    let transaction0 :: STM ()
        transaction0 = do
            balance <- readTVar balanceRef
            let newBalance = balance - 10
            writeTVar balanceRef newBalance
            if (0 <= newBalance) then return () else empty

    let transaction1 :: STM ()
        transaction1 = do
            balance <- readTVar balanceRef
            let newBalance = balance + 3
            writeTVar balanceRef newBalance

    replicateM_ 1000 (forkIO (atomically (transaction0 <|> transaction1)))
    balance <- atomically (readTVar balanceRef)
    print balance
```

# `STM` is a `Monoid`

Note that `STM` technically does not implement `Monoid`

`STM` implements `Alternative` (which is basically the same thing as `Monoid`)

We'll wrap `STM` in a `newtype` to provide our own non-orphan `Monoid` instance

```haskell
newtype Transaction a = Transaction { getTransaction :: STM a }

instance Monoid (Transaction a) where
    mempty = Transaction retry

    mappend (Transaction l) (Transaction r) = Transaction (l `orElse` r)
```

This instance satisfies the `Monoid` laws because:

```haskell
retry `orElse` x = x                                   -- mempty <> x = x

x `orElse` retry = x                                   -- x <> mempty = x

(x `orElse` y) `orElse` z = x `orElse` (y `orElse` z)  -- (x <> y) <> z = x <> (y <> z)
```

# `Managed` is an `Applicative` and `Monad`

```haskell
-- | A managed resource that you acquire using `with`
newtype Managed a = Managed { (>>-) :: forall r . (a -> IO r) -> IO r }

instance Functor Managed where
    fmap f mx = Managed (\return_ ->
        mx >>- \x ->
        return_ (f x) )

instance Applicative Managed where
    pure r    = Managed (\return_ ->
        return_ r )

    mf <*> mx = Managed (\return_ ->
        mf >>- \f ->
        mx >>- \x ->
        return_ (f x) )

instance Monad Managed where
    return r = Managed (\return_ ->
        return_ r )

    ma >>= f = Managed (\return_ ->
        ma  >>- \a ->
        f a >>- \b ->
        return_ b )

instance Monoid a => Monoid (Managed a) where
    mempty = pure mempty

    mappend = liftA2 mappend
```

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

# `Applicative`s are `Composable`

Haskell's standard library provides a type for "composing" `Applicative`s:

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative

newtype (f `O` g) a = Compose { unCompose :: f (g a) } deriving (Functor)

instance (Applicative f, Applicative g) => Applicative (f `O` g) where
    pure x = Compose (pure (pure x))

    Compose l <*> Compose r = Compose (liftA2 (<*>) l r)
```

... which has the interesting property that:

```haskell
f `O` Identity ≅ f                 -- (f . id) = f

Identity `O` f ≅ f                 -- (id . f) = f

(f `O` g) `O` h ≅ f `O` (g `O` h)  -- (f . g) . h = f . (g . h)
```

# Conclusion


Let's revisit our original goals:

* improved code reasoning skills
    * Our program is a modular sum of its parts
* faster prototyping
    * We can easily add/remove composable components
* simpler and more coherent APIs
    * 
* fewer corner cases
    * Our proofs were correct by construction
* ease of code maintenance

Abstractions from category theory and abstract algebra help decompose programs
into composable building blocks

We've talked about programs that are composable in two senses of the word:

* You can compose anything that implements `Monoid`
* You can build a new `Monoid` by composing `Applicative`s

# TODO

* Talk about how `Applicative`s let you lift other operations, like numeric
  operations and `IsString`
