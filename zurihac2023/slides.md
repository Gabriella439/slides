% Monad transformers are good, actually
% Gabriella Gonzalez
% June ???, 2023

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

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
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
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
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

… but in a sec I will argue it's <span style="color:#5BCEFA">still better</span>

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

## With StateT

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

# TODO

- Explain the difference between `transformers` and `mtl`
