% Monad transformers are good, actually
% Gabriella Gonzalez
% June 12, 2023

# Goals

<style>
.reveal h1, .reveal h2, .reveal h3, .reveal h4, .reveal h5 {
  text-transform: none;
}

section {
  overflow-y: auto !important;
  overflow-x: hidden !important;
  height: 100%;
}

code {
  max-height: none !important;
}

.reveal p {
  text-align: left;
}
.reveal ul {
  display: block;
}
.reveal ol {
  display: block;
}

code:not(.sourceCode) {
  color: #5BCEFA !important;
}
</style>

The goals behind this talk are to:

- motivate monad transformers
- explain how to use them
- dispel some common myths

## Motivation

My audience will likely have two types of listeners:

- **<span style="color:#FFFFFF">those who have never heard of monad transformers</span>**

  Lots of professional haskellers in this category!

- **<span style="color:#FFFFFF">those who assume everyone's heard of them<span>**

  This was me for the longest time 😅

Hopefully fewer people in both categories after today

## Prior knowledge

I'm assuming prior knowledge of Haskell basics

I'm **<span style="color:#F5A9B8">NOT</span>** assuming you know monad transformers

I'm going to teach monad transformers <span style="color:#5BCEFA">from scratch</span>

## Title

The title of this talk is tongue-in-cheek

It's pretty hip to dunk on monad transformers

… but monad transformers are good, actually!

# Outline

* **<span style="color:#FFFFFF">What are monad transformers for?</span>**
* Tutorial
* Tour of common transformers
* `transformers` versus `mtl`
* Common misconceptions

## Monad transformers by example

```haskell
import Text.Read (readMaybe)

readInt :: IO (Maybe Int)
readInt = do
    string <- getLine
    return (readMaybe string)

readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = do
    maybeInt0 <- readInt
    case maybeInt0 of
        Nothing -> return Nothing
        Just int0 -> do
            maybeInt1 <- readInt
            case maybeInt1 of
                Nothing -> return Nothing
                Just int1 -> do
                    maybeInt2 <- readInt
                    case maybeInt2 of
                        Nothing -> return Nothing
                        Just int2 -> do
                            return (Just (int0, int1, int2))
main :: IO ()
main = do
    maybeThreeInts <- readThreeInts
    print maybeThreeInts
```

## Using MaybeT

```haskell
{-# LANGUAGE BlockArguments #-}

import Control.Monad.Trans.Maybe (MaybeT(..))
import Text.Read (readMaybe)

readInt :: IO (Maybe Int)
readInt = do
    string <- getLine
    return (readMaybe string)

readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT do
    int0 <- MaybeT readInt
    int1 <- MaybeT readInt
    int2 <- MaybeT readInt
    return (int0, int1, int2)
```

## Looping

```haskell
import Text.Read (readMaybe)

readInt :: IO (Maybe Int)
readInt = do
    string <- getLine
    return (readMaybe string)

ensureInt :: IO (Maybe Int)
ensureInt = do
    maybeInt <- ensureInt
    case maybeInt of
        Nothing -> ensureInt
        Just n  -> return (Just n)
```

## Without MaybeT

```haskell
import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Text.Read (readMaybe) 

readInt :: IO (Maybe Int)
readInt = do
    string <- getLine
    return (readMaybe string)

ensureInt :: IO (Maybe Int)
ensureInt = runMaybeT loop
  where
    loop = MaybeT readInt <|> loop
```

## What is the pattern?

- Take some code of interest (e.g. `readInt`)

- Wrap that in a monad transformer (e.g. `MaybeT`)

- **<span style="color: #FFFFFF">Unlock new functionality (e.g. `do` notation, `(<|>)`)</span>**

- Unwrap the monad transformer (e.g. `runMaybeT`)

The third step is the key bit!

## Another example

```haskell
{-# LANGUAGE BlockArguments #-}

module Example where

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

char :: Char -> Parser ()
char c = Parser \string -> case string of
    s : tring | c == s -> [((), tring)]
    _                  -> []

string :: String -> Parser ()
string = mapM_ char

example :: Parser ()
example = do
    string "i like "
    string "cats" <|> string "dogs"
```

## Instances

```haskell
import Control.Applicative (Alternative(..))
import Control.Monad (ap, liftM)

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = Parser (\string -> [(a, string)])

    (<*>) = ap

instance Monad Parser where
    m >>= f = Parser \string -> do
        (a, remainder) <- runParser m string
        runParser (f a) remainder

instance Alternative Parser where
    empty = Parser mempty

    Parser l <|> Parser r = Parser (l <> r)
```

## With StateT (Part I)

```haskell
instance Functor Parser where
    fmap f (Parser k) = Parser (runStateT (fmap f (StateT k)))

instance Applicative Parser where
    pure a = Parser (runStateT (pure a))

    Parser f <*> Parser x = Parser (runStateT (StateT f <*> StateT x))

instance Monad Parser where
    m >>= f =
        Parser (runStateT (StateT (runParser m) >>= (StateT . runParser . f)))

instance Alternative Parser where
    empty = Parser (runStateT empty)

    Parser l <|> Parser r = Parser (runStateT (StateT l <|> StateT r))
```

## With StateT (Part II)

```haskell
{-# LANGUAGE DerivingVia #-}

import Control.Applicative (Alternative(..))
import Control.Monad.Trans.State (StateT(..))

newtype Parser a = Parser { runParser :: String -> [(a, String)] }
    deriving (Functor, Applicative, Monad, Alternative)
        via (StateT String [])
```

<ul>
<li class="fragment">Take some code of interest (e.g. `Parser`)</li>
<li class="fragment">Wrap that in a monad transformer (e.g. `StateT`)</li>
<li class="fragment">**<span style="color: #FFFFFF">Unlock new functionality (e.g. instances)</span>**</li>
<li class="fragment">Unwrap the monad transformer</li>
</ul>

# Outline

* What are monad transformers for?
* **<span style="color:#FFFFFF">Tutorial</span>**
* Tour of common transformers
* `transformers` versus `mtl`
* Common misconceptions

## Revisiting MaybeT example

```haskell
{-# LANGUAGE BlockArguments #-}

import Control.Monad.Trans.Maybe (MaybeT(..))
import Text.Read (readMaybe)

readInt :: IO (Maybe Int)
readInt = do
    string <- getLine
    return (readMaybe string)

readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT do
    int0 <- MaybeT readInt
    int1 <- MaybeT readInt
    int2 <- MaybeT readInt
    return (int0, int1, int2)
```

Why does this work?

## MaybeT datatype definition

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

In other words:

```haskell
MaybeT :: m (Maybe a) -> MaybeT m a

runMaybeT :: MaybeT m a -> m (Maybe a)
```

`m` can be any type constructor that implements `Monad`

… like `IO`!

## Specializing to IO

```haskell
newtype MaybeT IO a = MaybeT { runMaybeT :: IO (Maybe a) }
```

In other words:

```haskell
MaybeT :: IO (Maybe a) -> MaybeT IO a

runMaybeT :: MaybeT IO a -> IO (Maybe a)
```

For example:

```haskell
readInt :: IO (Maybe Int)

MaybeT readInt :: MaybeT IO Int
```

## Walking through the types

```haskell
readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT ((do
    int0 <- (MaybeT readInt :: MaybeT IO Int)
    int1 <- (MaybeT readInt :: MaybeT IO Int)
    int2 <- (MaybeT readInt :: MaybeT IO Int)
    return (int0, int1, int2)) :: MaybeT IO (Int, Int, Int))
```

This only works if `MaybeT IO` has a `Monad` instance

## Monad instance for MaybeT

```haskell
{-# BlockArguments #-}

instance Monad m => Monad (MaybeT m) where
    return x = MaybeT (return (Just x))

    m >>= f = MaybeT do
        maybeX <- runMaybeT m
        case maybeX of
            Nothing -> return Nothing
            Just x  -> runMaybeT (f x)
```

This is essentially the same as our first attempt:

```haskell
readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = do
    maybeInt0 <- readInt
    case maybeInt0 of
        Nothing -> return Nothing
        Just int0 -> do
            maybeInt1 <- readInt
            case maybeInt1 of
                Nothing -> return Nothing
                Just int1 -> do
                    maybeInt2 <- readInt
                    case maybeInt2 of
                        Nothing -> return Nothing
                        Just int2 -> do
                            return (Just (int0, int1, int2))
```

## Monad transformer (Part I)

`MaybeT` is called a "monad transformer"

A "monad transformer" is like a type-level function:

- the input is a simpler `Monad` (e.g. `m`)
- the output is a more complex `Monad` (e.g. `MaybeT m`)

## Monad transformer (Part II)

In fact, `MaybeT` is (essentially) a type-level function:

```haskell
MaybeT :: (* -> *) -> (* -> *)
--         ↑↑↑↑↑↑      ↑↑↑↑↑↑
--         Input       Output

m        :: * -> *  -- `m` is a `Monad`

MaybeT m :: * -> *  -- `MaybeT m` is also a `Monad`

instance Monad m => Monad (MaybeT m)
```

`MaybeT` is a function from a simpler `Monad` (`m`) to a more complex `Monad`
(`MaybeT m`)

## Monad transformer class

Every monad transformer implements `MonadTrans`:

```haskell
class (forall m. Monad m => Monad (t m)) => MonadTrans t where
    lift :: Monad m => m a -> t m a
```

```haskell
instance MonadTrans MaybeT where
    lift m = MaybeT (fmap Just m)
```

Specializing to `MaybeT IO`:

```haskell
lift :: Monad m  => m  a -> t      m  a
lift :: Monad IO => IO a -> MaybeT IO a
```

## Motivating lift (Part I)

```haskell
readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT do
    int0 <- MaybeT readInt
    -- Suppose I wanted to print `int0` here.  How would I do that?
    int1 <- MaybeT readInt
    int2 <- MaybeT readInt
    return (int0, int1, int2)
```

## Motivating lift (Part II)

```haskell
readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT do
    int0 <- MaybeT readInt
    -- Wrong!  Needs a type like `MaybeT IO …`
    (print int0 :: IO ())
    int1 <- MaybeT readInt
    int2 <- MaybeT readInt
    return (int0, int1, int2)
```

## Motivating lift (Part III)

```haskell
readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT do
    int0 <- MaybeT readInt
    -- Wrong!  Argument to `MaybeT` needs type like `IO (Maybe …)`
    MaybeT (print int0 :: IO ())
    int1 <- MaybeT readInt
    int2 <- MaybeT readInt
    return (int0, int1, int2)
```

## Motivating lift (Part IV)

```haskell
readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT do
    int0 <- MaybeT readInt
    -- Correct, but we can improve upon this further!
    MaybeT (fmap Just (print int0))
    int1 <- MaybeT readInt
    int2 <- MaybeT readInt
    return (int0, int1, int2)
```

This works because:

```haskell
print int0 :: IO ()

fmap Just (print int0) :: IO (Maybe ())

MaybeT (fmap Just (print int0)) :: MaybeT IO ()
```

## Motivating lift (Part V)

```haskell
readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT do
    int0 <- MaybeT readInt
    -- That's the same thing as `lift`!
    lift (print int0)
    int1 <- MaybeT readInt
    int2 <- MaybeT readInt
    return (int0, int1, int2)
```

```haskell
lift m = MaybeT (fmap Just m)

lift (print int0) = MaybeT (fmap Just (print int0))
```

## Etymology of lift

We "lift" the simpler `Monad` into the more complex `Monad`

```haskell
lift :: IO a -> MaybeT IO a
```

(really a lot of Haskell functions could be called `lift`)

## Recap

This is the way all monad transformers work:

- Extend the base `Monad` (`m`) with a new feature
- Add a `Monad` instance (so `do` notation still works)
- Add a `MonadTrans` instance (for `lift`)

# Outline

* What are monad transformers for?
* Tutorial
* **<span style="color:#FFFFFF">Tour of common transformers</span>**
* `transformers` versus `mtl`
* Common misconceptions

## The Unix philosophy

Each standard monad transformer *<span style="color:#F5A9B8">does one thing and does it well</span>*

They each add some new feature to the underlying ("base") `Monad`

You can stack as many of them as you want to keep adding new features

## IdentityT

```haskell
-- The simplest monad transformer of them all
--
-- Does not extend the base monad with any new features
newtype IdentityT m a = IdentityT{ runIdentityT :: m a }

-- We can only do whatever the base monad can do (using `lift`)
example :: IdentityT IO ()
example = do
    lift (print 2)  -- Output: 2
    lift (print 3)  -- Output: 3

main :: IO ()
main = runIdentityT example
```

## ReaderT

```haskell
-- Commonly used for threading a read-only configuration (`r`)
newtype ReaderT r m a = ReaderT{ runReaderT :: r -> m a }

-- We can query the read-only configuration using `ask`
ask :: Monad m => ReaderT r m r
ask = ReaderT return

example :: ReaderT Int IO ()
example = do
    n <- ask        -- Now we can do something new!
    lift (print n)  -- Output: 2

main :: IO ()
main = runReaderT example 2
```

## WriterT

```haskell
-- Commonly used for outputting write-only `Monoid` (`w`)
newtype WriterT w m a = WriterT{ runWriterT :: m (a, w) }

-- We can output a write-only value using `tell`
tell :: Monad m => w -> WriterT w m ()
tell w = WriterT (return ((), w))

example :: WriterT [Int] IO ()
example = do
    tell [2, 3]
    lift (print 1)  -- Output: 1
    tell [5]

main :: IO ()
main = do
    ((), numbers) <- runWriterT example
    print numbers  -- Output: [ 2, 3, 5 ]
```

## StateT

```haskell
-- Used for a readable and writable state (`s`)
newtype StateT s m a = StateT{ runStateT :: s -> m (a, s) }

put :: Monad m => s -> StateT s m ()
put s = StateT (\_ -> return ((), s))

get :: Monad m => StateT s m s
get = StateT (\s -> return (s, s))

example :: StateT Int IO ()
example = do
    n <- get
    lift (print n)  -- Output: 2
    put (n + 1)

main :: IO ()
main = do
    m <- runStateT example 2
    print m  -- 3
```

## ExceptT

```haskell
-- Commonly used for throwing and catching errors
newtype ExceptT e m a = ExceptT{ runExceptT :: m (Either e a) }

throwE :: Monad m => e -> ExceptT e m a
throwE e = ExceptT (return (Left e))

catchE
    :: Monad m
    => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` f = ExceptT do
    e <- runExceptT m
    case e of
        Left  l -> runExceptT (f l)
        Right r -> return (Right r)

example :: ExceptT String IO ()
example = do
    liftIO (print 1)  -- Output: 1
    throwE "Oops!"
    liftIO (print 2)  -- Not reachable

main :: IO ()
main = do
    e <- runExceptT example
    print e  -- Output: "Oops!"
```

## Similarity

Monad transformers resemble simpler analogs:

```haskell
{-# LANGUAGE BlockArguments #-}

newtype State s a = State{ runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State (\s -> (a, s))

    m >>= f = State \s ->
        let (a, s') = runState m s
        in  runState (f a) s'

newtype StateT s m a = StateT{ runStateT :: s -> m (a, s) }

instance Monad m => Monad (StateT s m) where
    return a = StateT (\s -> return (a, s))

    m >>= f = StateT \s -> do
        (a, s') <- runStateT m s
        runStateT (f a) s'
```

## Identity

The real implementation of `State` is:

```haskell
newtype Identity a = Identity{ runIdentity :: a }

type State s = StateT s Identity
```

Many monads are special cases of a transformer:

```haskell
type Reader    r = ReaderT   w Identity
type Writer    w = WriterT   w Identity
type Cont      r = ContT     r Identity
type ParsecT e s = ParsecT e s Identity
```

## Exceptions - Ergonomics

Not all monads are special cases of transformers

Some exceptions:

- <span style="color: #FFFFFF">`Maybe`</span> - `MaybeT Identity` less ergonomic
- <span style="color: #FFFFFF">`Either`</span> - `ExceptT Identity` less ergonomic
- <span style="color: #FFFFFF">`[]`</span> - `ListT Identity` less ergonomic
- <span style="color: #FFFFFF">`IO`</span> - no such thing as `IOT`
- <span style="color: #FFFFFF">`STM`</span> - no such thing as `STMT`
- <span style="color: #FFFFFF">`Identity`</span> - take a second to think about this one

# Outline

* What are monad transformers for?
* Tutorial
* Tour of common transformers
* **<span style="color:#FFFFFF">`transformers` versus `mtl`</span>**
* Common misconceptions

## transformers

So far, everything we've covered is from the `transformers` package:

- Concrete monad transformers (e.g. `MaybeT`)
- Their simpler analogs (e.g. `State`)
- Useful utilities for each transformer (e.g. `ask`)
- The `MonadTrans` class (i.e. `lift`)

So why is there another package called `mtl`?

Which package is the "real" monad transformers package?

## Motivating mtl

Monad transformers are awkward to nest

```haskell
example :: StateT Int (ExceptT String IO) ()
example = do
    n <- get

    if n < 0
        then lift (throwE "Oh no!")
        else lift (lift (print "Hooray!"))
```

We have to add a `lift` for every layer we "dig down"

- 0 layers down (`get` / `StateT`) - 0 `lift` requireds
- 1 layer down (`throwE` / `ExceptT`) - 1 `lift` required
- 2 layers down (`print` / `IO`) - 2 `lift`s required

## MonadIO

The `mtl` package solves this problem using typeclasses

```haskell
class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO io = io

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO io = lift (liftIO io)

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO io = lift (liftIO io)

instance MonadIO m => MonadIO (StateT s m) where
    liftIO io = lift (liftIO io)

instance MonadIO m => MonadIO (MaybeT e m) where
    liftIO io = lift (liftIO io)

instance MonadIO m => MonadIO (ExceptT e m) where
    liftIO io = lift (liftIO io)
```

## MonadIO - Example

```haskell
example :: StateT Int (ExceptT String IO) ()
example = do
    n <- get

    if n < 0
        then lift (throwE "Oh no!")
        else liftIO (print "Hooray!")
          -- ↑ Now we only need a single `liftIO`
```

## MonadState

```haskell
class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()

instance Monad m => MonadState s (StateT s m) where
    get = Control.Monad.Trans.State.get  -- From `transformers`
    put = Control.Monad.Trans.State.put  -- From `transformers`

instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put s = lift (put s)

instance (Monoid w, MonadState s m) => MonadState s (Writer w m) where
    get = lift get
    put s = lift (put s)

instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put s = lift (put s)

instance MonadState s m => MonadState s (ExceptT m) where
    get = lift get
    put s = lift (put s)
```

## MonadState - Example

```haskell
example :: StateT Int (ExceptT String IO) ()
example = do
    n <- get
      -- ↑ No change.  It wasn't lifted anyway

    if n < 0
        then lift (throwE "Oh no!")
        else liftIO (print "Hooray!")
```

## MonadError

```haskell
class Monad m => MonadError e m where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a 

instance Monad m => MonadError e (ExceptT e m) where
    throwError = Control.Monad.Trans.Except.throwE
    catchError = Control.Monad.Trans.Except.catchE

-- But wait, now it gets more tricky!
instance MonadError e m => MonadError e (ReaderT r m) where
    throwError e = lift (throwError e)
    catchError = Control.Monad.Trans.Reader.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
    throwError e = lift (throwError e)
    catchError = Control.Monad.Trans.Writer.liftCatch catchError

instance MonadError e m => MonadError e (StateT s m) where
    throwError e = lift (throwError e)
    catchError = Control.Monad.Trans.State.liftCatch catchError

instance MonadError e m => MonadError e (MaybeT m) where
    throwError e = lift (throwError e)
    catchError = Control.Monad.Trans.Maybe.liftCatch catchError
```

## MonadError - Example

```haskell
example :: StateT Int (ExceptT String IO) ()
example = do
    n <- get

    if n < 0
        then throwE "Oh no!"
        -- ↑ No change.  It wasn't lifted anyway
        else liftIO (print "Hooray!")
```

We can't get rid of the `liftIO` as easily as `lift`

We'd have to generalize `IO` utilities to use `liftIO`

There is a package that does this! (`unliftio`)

## MaybeT

Why does the `mtl` not have a `MonadMaybe` class?

Why isn't there a `Control.Monad.Maybe`?

`Alternative`/`MonadPlus` is already that class!

```haskell
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance Monad m => MonadPlus (MaybeT m) where …
instance MonadPlus m => MonadPlus (ReaderT r m) where
instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
instance MonadPlus m => MonadPlus (StateT s m) where
```

## What is the mtl?

The `mtl` is just an extension to the `transformers` package

The `mtl` re-exports most of the `transformers` package

Pedantically both packages are "monad transformers"

It's better to refer to the packages by name to avoid confusion

## Differences

The main differences between the two packages are:

- `mtl` utilities use type classes

  ```haskell
  -- transformers
  ask :: Monad m => ReaderT r m r

  -- `mtl`
  ask :: MonadReader r m => m r
  ```

- `mtl` module names are shorter

  e.g. `Control.Monad.State` instead of `Control.Monad.Trans.State`

## Use cases

`transformers` often used in short / localized way

```haskell
readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT do
    int0 <- MaybeT readInt
    int1 <- MaybeT readInt
    int2 <- MaybeT readInt
    return (int0, int1, int2)
```

`mtl` often used in long-lived / global way

```haskell
newtype App a = App{ runApp :: ReaderT Config (MaybeT IO) a }
    deriving
        ( Functor, Applicative, Monad
        , Alternative, MonadPlus  -- lifted from MaybeT
        , MonadIO                 -- lifted from IO
        , MonadReader Config      -- lifted from ReaderT
        )
```

# Outline

* What are monad transformers for?
* Tutorial
* Tour of common transformers
* `transformers` versus `mtl`
* **<span style="color:#FFFFFF">Common misconceptions</span>**

## "Monad transformers are slow"

This originates from Alexis's [Effects for less](https://www.youtube.com/watch?v=0jI-AlWEwYI)

Most people misunderstood the actual claim she made

`mtl` slower than `eff` if you don't mark code `INLINABLE`

`mtl` fastest if you mark code `INLINABLE`

… according to her own benchmarks

In other words, `mtl` has highest performance ceiling!

## "Never use X monad transformer"

… where `X = { ExceptT, StateT, WriterT }`

This advice is (IMO) correct if your base monad is `IO`

… but they're still useful with other base monads!

```haskell
newtype Parser a = Parser { runParser :: String -> [(a, String)] }
    deriving (Functor, Applicative, Monad, Alternative)
        via (StateT String [])
```

## "All we need are Reader/Writer/State/Except"

Algebraic effects discussions often ignore these important monad transformers:

- `ListT` "done right" - list monad transformer
- `FreeT` (free monad transformer)
- `ContT` (continuation monad transformer)

… because they're inconvenient for them to model

`transformers` and `mtl` handle them just fine!

# TODO

- Explain the difference between `transformers` and `mtl`
- Explain how not all `Monad`s can be turned into monad transformers
  - and include some examples (e.g. `IO` / `STM`)
- Link to social media
