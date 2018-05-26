% Parallel state machines
% Gabriel Gonzalez
% April 18, 2017

# Overview

This talk is based on the following paper:

> [Mytkowicz, Todd, Madanlal Musuvathi, and Wolfram Schulte. "Data-parallel finite-state machines." ACM SIGARCH Computer Architecture News. Vol. 42. No. 1. ACM, 2014](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/asplos302-mytkowicz.pdf)

... and also a 🔥 blazing 🔥 fast signature detection package named `sig`

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">
Tip when evaluating libraries: check if it&#39;s blazing fast. If it&#39;s fast, but the README doesn&#39;t specify whether its fastness is blazing, keep searching. Often you can find a similar library that does the same thing, but blazingly. Blazing means good.</p>&mdash; Andrew Clark (@acdlite) <a href="https://twitter.com/acdlite/status/974390255393505280?ref_src=twsrc%5Etfw">March 15, 2018</a>
</blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

`sig` is based on a subset of the paper (simulating up to 16 states in parallel)

# Data-Parallel Finite-State Machines

![](./paper.png)

The paper describes how to simulate state machines:

* ... quickly
* ... and in parallel

# State machine

According to [Wikipedia](https://en.wikipedia.org/wiki/Finite-state_machine):

> A deterministic finite state machine or acceptor deterministic finite state
> machine is a quintuple (Σ, S, s₀, δ, F), where: 
> 
> * Σ is the input alphabet (a finite, non-empty set of symbols).
> * S is a finite, non-empty set of states
> * s₀ is an initial state, an element of S
> * δ is the state-transition function: δ : S × Σ → S
> * F is the set of final states, a (possibly empty) subset of S

```haskell
import Data.Set (Set)

data StateMachine _Σ _S = StateMachine
    { s₀ :: _S
    , δ  :: (_S, _Σ) -> _S
    , _F :: Set _S
    }
```

# Use English names

Let's translate this:

```haskell
import Data.Set (Set)

data StateMachine _Σ _S = StateMachine
    { s₀ :: _S
    , δ  :: (_S, _Σ) -> _S
    , _F :: Set _S
    }
```

... to a version with English names:

```haskell
import Data.Set (Set)

data StateMachine input state = StateMachine
    { startingState   :: state
    , step            :: state -> input -> state
    , acceptingStates :: Set state
    }
```

# Hide the state

Let's translate this:

```haskell
import Data.Set (Set)

data StateMachine input state = StateMachine
    { startingState   :: state
    , step            :: state -> input -> state
    , acceptingStates :: Set state
    }
```

... to this version that hides the type of the internal state:

```haskell
{-# LANGUAGE ExistentialQuantification #-}

import Data.Set (Set)

data StateMachine input = forall state . Ord state => StateMachine
    { startingState   :: state
    , step            :: state -> input -> state
    , acceptingStates :: Set state
    }
```

# Example state machine - Diagram

![](cstyle.png)

# Example state machine - Code

```haskell
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Set

data State = A | B | C | D deriving (Eq, Ord)

cStyleComments :: StateMachine Char
cStyleComments = StateMachine {..}
  where
    startingState = A

    step '/' A = B
    step '/' B = B
    step '/' C = C
    step '/' D = A

    step '*' A = A
    step '*' B = C
    step '*' C = D
    step '*' D = D

    step  _  A = A
    step  _  B = A
    step  _  C = C
    step  _  D = C

    acceptingStates = Data.Set.fromList [ A, B ]
```

# Run a state machine

```haskell
{-# LANGUAGE ExistentialQuantification #-}

import Data.Set (Set)

import qualified Data.Set

data StateMachine input = forall state . Ord state => StateMachine
    { startingState   :: state
    , step            :: input -> state -> state
    , acceptingStates :: Set state
    }

accept :: StateMachine input -> [input] -> Bool
accept (StateMachine {..}) startingInputs = 
    Data.Set.member finalState acceptingStates
  where
    finalState = loop startingInputs startingState

    loop [] state = state

    loop (input:inputs) state = loop inputs (step input state)

--  loop [i₀, i₁, i₂] state = step i₂ (step i₁ (step i₀ state))
```

# Example runs

```haskell
>>> accept cStyleComments "/**/"
True
>>> accept cStyleComments "/*ABC"
False
>>> accept cStyleComments "/**"
False
>>> accept cStyleComments "ABC"
True
>>> accept cStyleComments "**/"
True
>>> accept cStyleComments "/* /* */"
True
```

# Question

How do we parallelize `accept`?

```haskell
accept :: StateMachine input -> [input] -> Bool
accept (StateMachine {..}) startingInputs = 
    Data.Set.member finalState acceptingStates
  where
    finalState = loop startingInputs startingState

    loop [] state = state

    loop (input:inputs) state = loop inputs (step input state)
```

# Trick #0 - Restrict the number of states

We can simulate every state in parallel if we restrict the number of states

Let's cap the number of states to 16 (for now):

```haskell
data State = S00 | S01 | S02 | S03 | S04 | S05 | S06 | S07
           | S08 | S09 | S10 | S11 | S12 | S13 | S14 | S15
           deriving (Eq, Ord)
```

... and update our `StateMachine` type to only work on `State`s:

```haskell
data StateMachine input = StateMachine
    { startingState   :: State
    , step            :: input -> State -> State
    , acceptingStates :: Set State
    }
```

# State transitions

A state transition is a function from an old `State` to a new `State`:

```haskell
newtype Transition = Transition { runTransition :: State -> State }
```

... and a state transition is a `Monoid`:

```haskell
instance Monoid Transition where
    mempty = Transition id

    mappend (Transition f) (Transition g) = Transition (g . f)
```

... equivalent to the `Endo` `Monoid`, where:

* `mempty` is the empty transition that doesn't change the `State`
* `mappend` composes two `Transition`s

# Using `Transition`

We can refactor the inner loop of `accept` to use the `Transition` `Monoid`

```haskell
accept :: StateMachine input -> [input] -> Bool
accept (StateMachine {..}) startingInputs =
    Data.Set.member finalState acceptingStates
  where
    finalState = loop startingInputs startingState

--  loop :: [input] -> State -> State
    loop [] state = state

    loop (input:inputs) state =
        loop inputs (step input state)

--  loop [i₀, i₁, i₂] state = step i₂ (step i₁ (step i₀ state))
```

First, we refactor the inner loop to use `id` and `(.)`:

```haskell
--  loop :: [input] -> State -> State
    loop [] state = id state

    loop (input:inputs) state =
        (loop inputs . step input) state

--  loop [i₀, i₁, i₂] state = (id . step i₂ . step i₁ . step i₀) state
```

Then we η-reduce ("eta"-reduce) the `loop` function:

```haskell
--  loop :: [input] -> State -> State
    loop [] = id 

    loop (input:inputs) =
        loop inputs . step input

--  loop [i₀, i₁, i₂] = id . step i₂ . step i₁ . step i₀
```

Now we wrap things in `Transition` to use `mempty` and `(<>)` instead:

```haskell
--  loop :: [input] -> Transition
    loop [] = mempty

    loop (input:inputs) =
        Transition (step input) <> loop inputs

--  loop [i₀, i₁, i₂] =
--          Transition (step i₀)
--      <>  Transition (step i₁)
--      <>  Transition (step i₂)
--      <>  mempty
```

# Parallel `mappend`

The `(<>)` operator is associative:

```haskell
(x <> y) <> z = x <> (y <> z)
```

... so if we have a large chain of `(<>)`s, like this:

```haskell
x₀ <> x₁ <> x₂ <> x₃ <> x₄ <> x₅ <> x₆ <> x₇
```

... we can divide up that computational work into two smaller parallel batches:

```haskell
let l = x₀ <> x₁ <> x₂ <> x₃

    r = x₄ <> x₅ <> x₆ <> x₇

in  l `par` r `par` l <> r
```

... and if we can afford more parallelism then we can further sub-divide

# Parallelize functions?

However, that doesn't really work for our inner `loop`, though:

```haskell
loop [i₀, i₁, i₂, i₃] =
        Transition (step i₀)
    <>  Transition (step i₁)
    <>  Transition (step i₂)
    <>  Transition (step i₃)
```

We might naïvely try to do this:

```haskell
let l = Transition (step i₀) <> Transition (step i₁)

    r = Transition (step i₂) <> Transition (step i₃)

in  l `par` r `par` l <> r
```

... but we can't really "evaluate" an unsaturated function call ahead of time

... or can we?

# Memoization

Our functions only accept 16 possible inputs, so we can precompute all outputs

```haskell
precompute :: 
```

We can encode a state transition using a different representation

# Parallelizing things

The reason we do so is because now we can use `mconcat` and `map`:

```haskell
--  loop :: [input] -> Transition
    loop inputs = mconcat (map (Transition . step) inputs)

--  loop [i₀, i₁, i₂] =
--      mconcat (map (Transition . step) [i₀, i₁, i₂])
```

... which is the same thing as `foldMap`:

```haskell
--  loop :: [input] -> Transition
    loop inputs = foldMap (Transition . step) inputs

--  loop [i₀, i₁, i₂] inputs = foldMap (Transition . step) [i₀, i₁, i₂] inputs
```

... and we can η-reduce again!

```haskell
--  loop :: [input] -> Transition
    loop = foldMap (Transition . step)
```

The final `accept` function:

```haskell
accept :: forall input . StateMachine input -> [input] -> Bool
accept (StateMachine {..}) startingInputs =
    Data.Set.member finalState acceptingStates
  where
    finalState = runTransition (loop startingInputs) startingState

    loop :: [input] -> Transition
    loop = foldMap (Transition . step)
```

# Insight

The paper shows that we can efficiently simulate up to 16 states at a time

... in one CPU instruction!!

[`PSHUFB`](https://en.wikipedia.org/wiki/Finite-state_machine) is short for
"packed shuffle bytes"

We can understand how `PSHUFB` works by analogy to our Haskell implementation

# Encoding state transitions

We can encode a `Transition` by storing where all 16 states transition to.

```haskell
instance Binary Transition where
    put (Transition f) = mapM_ (put . f) [minBound..maxBound]

    get = do
        let numStates = fromEnum (maxBound :: State) + 1
        !ss <- Data.Vector.replicateM numStates get
        return (Transition (\s -> ss ! fromEnum s))
```

Or in other words:

```haskell
    put (Transition f) = do
        put (f S00)
        put (f S01)
        put (f S02)
        put (f S03)
        ...
        put (f S15)
```
For example:

```
00000000  -- S0 → S0
00000001  -- S1 → S3
00000010  -- S3 → S2
00000011
00000100
00000101
00000110
00000111
00000000
00000001
00000010
00000011
00000100
00000101
00000110
00000111
```
You can encode a `Transition` in 64 bits!

# State

Let's define a Haskell data type representing one of 16 possible states:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

import Data.Binary (Binary)
import GHC.Generics (Generic)

data State = S00  -- 0000
           | S01  -- 0001
           | S02  -- 0010
           | S03  -- 0011
           | S04  -- 0100
           | S05  -- 0101
           | S06  -- 0110
           | S07  -- 0111
           | S08  -- 1000
           | S09  -- 1001
           | S10  -- 1010
           | S11  -- 1011
           | S12  -- 1100
           | S13  -- 1101
           | S14  -- 1110
           | S15  -- 1111
           deriving (Binary, Bounded, Enum, Generic)
```

We'll round up to 1 byte by deriving `Binary`


