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
- compare them to alternatives
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
* Comparison against algebraic effects
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

Using `MaybeT` is **<span style="color:#F5A9B8">NOT</span>** shorter

… but in a sec I will argue it's *still better*

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
* Comparison against algebraic effects
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

A "monad transformer" is like a function:

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

## Similarity

Monad transformers resemble simpler analogs:

```haskell
instance Monad Maybe where
    return x = Just x

    m >>= f =
        case m of
            Nothing -> Nothing
            Just x  -> f x

instance Monad m => Monad (MaybeT m) where
    return x = MaybeT (return (Just x))

    m >>= f = MaybeT do
        maybeX <- runMaybeT m
        case maybeX of
            Nothing -> return Nothing
            Just x  -> runMaybeT (f x)
```

## State and StateT

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
* **<span style="color:#FFFFFF">Comparison against algebraic effects</span>**
* Common misconceptions

# Outline

* What are monad transformers for?
* Tutorial
* Comparison against algebraic effects
* **<span style="color:#FFFFFF">Common misconceptions</span>**

# TODO

- Explain the difference between `transformers` and `mtl`
- Explain how not all `Monad`s can be turned into monad transformers
  - and include some examples (e.g. `IO` / `STM`)
- Link to social media
