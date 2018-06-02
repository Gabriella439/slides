% Parallel state machines
% Gabriel Gonzalez
% April 18, 2017

# Background

This talk is based on the following paper:

> [Mytkowicz, Todd, Madanlal Musuvathi, and Wolfram Schulte. "Data-parallel finite-state machines." ACM SIGARCH Computer Architecture News. Vol. 42. No. 1. ACM, 2014](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/asplos302-mytkowicz.pdf)

... and also a 🔥 blazing 🔥 fast signature detection package named `sig`

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">
Tip when evaluating libraries: check if it&#39;s blazing fast. If it&#39;s fast, but the README doesn&#39;t specify whether its fastness is blazing, keep searching. Often you can find a similar library that does the same thing, but blazingly. Blazing means good.</p>&mdash; Andrew Clark (@acdlite) <a href="https://twitter.com/acdlite/status/974390255393505280?ref_src=twsrc%5Etfw">March 15, 2018</a>
</blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

`sig` is based on a subset of the paper (simulating up to 16 states in parallel)

# Overview

* Haskell state machines
* C state machines
* Parallel state machines

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

    step A '/' = B
    step B '/' = B
    step C '/' = C
    step D '/' = A

    step A '*' = A
    step B '*' = C
    step C '*' = D
    step D '*' = D

    step A  _  = A
    step B  _  = A
    step C  _  = C
    step D  _  = C

    acceptingStates = Data.Set.fromList [ A, B ]
```

# Run a state machine

```haskell
{-# LANGUAGE ExistentialQuantification #-}

import Data.Set (Set)

import qualified Data.Set

data StateMachine input = forall state . Ord state => StateMachine
    { startingState   :: state
    , step            :: state -> input -> state
    , acceptingStates :: Set state
    }

accept :: StateMachine input -> [input] -> Bool
accept (StateMachine {..}) startingInputs = 
    Data.Set.member finalState acceptingStates
  where
    finalState = loop startingInputs startingState

    loop [] state = state

    loop (input:inputs) state = loop inputs (step state input)

--  loop [i₀, i₁, i₂] state = step (step (step state i₀) i₁) i₂
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

# Full example

```haskell
-- chars.hs

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import Data.Set (Set)

import qualified Data.Set

main :: IO ()
main = do
    string <- readFile "test.c" -- "/*" ++ replicate 1000000000 ' ' ++ "*/"
    print (accept cStyleComments string)

data StateMachine input = forall state . Ord state => StateMachine
    { startingState   :: state
    , step            :: state -> input -> state
    , acceptingStates :: Set state
    }

accept :: StateMachine input -> [input] -> Bool
accept (StateMachine {..}) startingInputs = 
    Data.Set.member finalState acceptingStates
  where
    finalState = loop startingInputs startingState

    loop [] state = state

    loop (input:inputs) state = loop inputs (step state input)

data State = A | B | C | D deriving (Eq, Ord)

cStyleComments :: StateMachine Char
cStyleComments = StateMachine {..}
  where
    startingState = A

    step A '/' = B
    step B '/' = B
    step C '/' = C
    step D '/' = A

    step A '*' = A
    step B '*' = C
    step C '*' = D
    step D '*' = D

    step A  _  = A
    step B  _  = A
    step C  _  = C
    step D  _  = C

    acceptingStates = Data.Set.fromList [ A, B ]
```

# Performance

```
$ time ./chars
True

real    0m7.406s
user    0m6.997s
sys     0m0.402s
```

1 B / 7.4 ns = 135 MB / s

# Using `ByteString`s

Haskell's `String` type is very inefficient (a linked-list of `Char`s)

We can speed things up by using `ByteString`s instead of `String`s

In fact, the `Data.ByteString` module has exactly the function we want:

```haskell
foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a 
```

... the correspondence is more obvious if we rename and document the type:

```haskell
foldl'
    :: (state -> Word8 -> state)
    -- ^ Step function
    -> state
    -- ^ Starting state
    -> ByteString
    -- ^ Input
    -> state
    -- ^ Final state
```

# `ByteString` example

```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Word (Word8)

import qualified Data.ByteString
import qualified Data.Set

main :: IO ()
main = do
    bytes <- Data.ByteString.readFile "test.c"
    print (accept cStyleComments bytes)

data StateMachine input = forall state . Ord state => StateMachine
    { startingState   :: state
    , step            :: state -> input -> state
    , acceptingStates :: Set state
    }

accept :: StateMachine Word8 -> ByteString -> Bool
accept (StateMachine {..}) bytes = 
    Data.Set.member finalState acceptingStates
  where
    finalState = Data.ByteString.foldl' step startingState bytes

data State = A | B | C | D deriving (Eq, Ord)

cStyleComments :: StateMachine Word8
cStyleComments = StateMachine {..}
  where
    startingState = A

    step A 47 = B
    step B 47 = B
    step C 47 = C
    step D 47 = A

    step A 42 = A
    step B 42 = C
    step C 42 = D
    step D 42 = D

    step A _  = A
    step B _  = A
    step C _  = C
    step D _  = C

    acceptingStates = Data.Set.fromList [ A, B ]
```

# Performance

```
$ time ./bytes
True

real    0m1.497s
user    0m0.705s
sys     0m0.760s
```

1 B / 1.5 ns = 667 MB / s

~4.9x faster using `ByteString` instead of `String`

# Question

Is 667 MB / s good?

Is 1 B / 1.5 ns good?

What is the single-threaded performance we should expect for an optimal state
machine implementation?

Assume a modern 3 GHz processor for simplicity

L1 cache reference: 0.5 ns

# C state machine

We can run a state machine in C using a lookup table for a fixed number of
states

We'll support up to 16 states for now:

```c
-- run.c

#include <stdlib.h>

#define NUM_STATES 16
#define NUM_BYTES  256

// Final state: [0..15]
unsigned char run(
    // Input bytes to process
    char *input,

    // Length of the input
    size_t length,

    // Starting state: [0..15]
    unsigned char startingState,

    // Step function encoded as lookup table
    char step[NUM_STATES][NUM_BYTES]
) {
    size_t i;
    char currentByte;
    unsigned char currentState = startingState;

    for (i = 0; i < length; i++) {
        currentByte = input[i];
        currentState = step[currentState][currentByte];
    }

    return currentState;
}
```

# Overview

# Pure C example

We can test a pure C example matching C-style comments as an example:

```c
-- file.c

#include <stdlib.h>
#include <stdio.h>

#define NUM_STATES 16
#define NUM_BYTES 256
#define INPUT_SIZE 1000000004

// Final state: [0..15]
unsigned char run(
    // Input bytes to process
    FILE *file,

    // Length of the input
    size_t length,

    // Starting state: [0..15]
    unsigned char startingState,

    // Step function encoded as lookup table
    unsigned char step[NUM_STATES][NUM_BYTES]
) {
    size_t i, j;
    char currentByte;
    unsigned char buffer[1000];
    unsigned char currentState = startingState;

    for (i = 0; i < INPUT_SIZE; i++) {
        currentByte = fgetc(file);
        currentState = step[currentState][currentByte];
    }

    return currentState;
}

int main() {
    size_t byte;
    unsigned char finalState;
    unsigned char startingState;
    unsigned char state;
    unsigned char cStyleComments[NUM_STATES][NUM_BYTES] = { 0 };
    FILE *file;

    for (byte = 0; byte < NUM_BYTES; byte++) {
        cStyleComments[0][byte] = 0;
        cStyleComments[1][byte] = 0;
        cStyleComments[2][byte] = 2;
        cStyleComments[3][byte] = 2;
    }
    cStyleComments[0][47] = 1;
    cStyleComments[1][47] = 1;
    cStyleComments[2][47] = 2;
    cStyleComments[3][47] = 0;

    cStyleComments[0][42] = 0;
    cStyleComments[1][42] = 2;
    cStyleComments[2][42] = 3;
    cStyleComments[3][42] = 3;

    startingState = 0;

    file = fopen("test.c", "rb");
    finalState = run(file, INPUT_SIZE, startingState, cStyleComments);
    fclose(file);
    if (finalState == 0 || finalState == 1) {
        printf("True");
    } else {
        printf("False");
    }
    return 0;
}
```

# Performance

Surprisingly, the pure C example is slower:

```
$ time ./file
True
real	0m38.809s
user	0m38.414s
sys	0m0.361s
```

1 B / 38.8 ns = 26 MB / s

I'm not sure why 🤷

You'll just have to trust me that using C will pay off in the end.  I promise

# Serialization

We can't use our C state machine until we serialize the Haskell state machine

However, we'll make a few changes to the Haskell types to simplify things:

```haskell
-- Hard-code the number of states to 16
data State = S00 | S01 | S02 | S03 | S04 | S05 | S06 | S07
           | S08 | S09 | S10 | S11 | S12 | S13 | S14 | S15

-- Split out a state transition into its own type
newtype Transition = Transition { runTransition :: State -> State }

-- Remove the `startingState` and `acceptingStates` fields from `StateMachine`
newtype StateMachine = StateMachine { runStateMachine :: Word8 -> Transition }
```

These types more closely match how the paper works

This choice of types also simplifies the serialization/deserialization logic

# Serializing `State`

We can derive `Generic` and `Binary` (and some other instances) for `State`:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

data State = S00 | S01 | S02 | S03 | S04 | S05 | S06 | S07
           | S08 | S09 | S10 | S11 | S12 | S13 | S14 | S15
           deriving (Binary, Bounded, Enum, Eq, Generic, Ord, Show)
```

The derived `Binary` instance encodes each state as one byte:

```haskell
>>> let bytes x = Data.ByteString.Lazy.unpack (Data.Binary.encode x)
>>> bytes S00
[0]
>>> bytes S01
[1]
>>> bytes S15
[15]
```

# Serializing `Transition`

We encode a state transition as a "transition array":

```haskell
{-# LANGUAGE BangPatterns #-}

import Data.Vector ((!))

import qualified Data.Vector

numberOfStates :: Int
numberOfStates = fromEnum (maxBound :: State) + 1

newtype Transition = Transition { runTransition :: State -> State }

instance Binary Transition where
    put (Transition f) = mapM_ (put . f) [minBound..maxBound]

    get = do
        !ss <- Data.Vector.replicateM numberOfStates get
        return (Transition (\s -> ss ! fromEnum s))
```

For every possible input state, we encode the respective output state, in order

# Example `Transition` encoding

For example, these transition functions:

```haskell
contrivedTransition :: Transition
contrivedTransition = Transition f
  where
    f S00 = S00
    f S01 = S04
    f S02 = S01
    f S03 = S10
    f S04 = S02
    f S05 = S00
    f S06 = S03
    f S07 = S06
    f S08 = S04
    f S09 = S12
    f S10 = S05
    f S11 = S02
    f S12 = S06
    f S13 = S08
    f S14 = S07
    f S15 = S14

-- Every state transitions to itself
identityTransition :: Transition
identityTransition = Transition id
```

... encode to these byte sequences:

```
>>> bytes transition
[0,4,1,10,2,0,3,6,4,12,5,2,6,8,7,14]
>>> bytes identityTransition
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
```

# Serializing `StateMachine`

We serialize `StateMachine`s the same way that we serialize `Transition`s

For every possible input byte, we encode the respective output transition, in
order

```haskell
import Data.Word (Word8)

newtype StateMachine = StateMachine { runStateMachine :: Word8 -> Transition }

instance Binary StateMachine where
    put (StateMachine k) = mapM_ (put . k) [minBound..maxBound]

    get = do
        let numBytes = fromEnum (maxBound :: Word8) + 1
        ts <- Data.Vector.replicateM numBytes get
        return (StateMachine (\word8 -> ts ! fromEnum word8))
```

# Example `StateMachine` encoding

```haskell
contrivedStateMachine :: StateMachine
contrivedStateMachine = StateMachine f
  where
    f byte = if even byte then contrivedTransition else identityTransition
```

```haskell
>>> bytes contrivedStateMachine 
[0,4,1,10,2,0,3,6,4,12,5,2,6,8,7,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0,4,…
```

This encoding uses 256 × 16 B = 4096 B = 4 KiB (compact! 😊)

# C-style comments

```haskell
buildStateMachine :: (Word8 -> State -> State) -> StateMachine
buildStateMachine f = StateMachine (fmap Transition f)

cStyleComments :: StateMachine
cStyleComments = buildStateMachine step
  where
    step 47 S00 = S01  -- Possible  comment start: Go to state #1
    step 42 S01 = S02  -- Confirmed comment start: Go to state #2
    step 42 S02 = S03  -- Possible  comment end  : Go to state #3
    step 47 S03 = S00  -- Confirmed comment end  : Go to state #0

    step 47 S01 = S01  -- Still might be a comment start: Stay on   state #1
    step  _ S01 = S00  -- Not a comment after all       : Return to state #0

    step 42 S03 = S03  -- Still might be a comment end  : Stay on   state #3
    step  _ S03 = S02  -- Not a comment after all       : Return to state #2

    step  _ S00 = S00  -- Outside of a comment: Stay on state #0

    step  _ S02 = S02  -- Inside a comment    : Stay on state #2

    step  _ _   = S00
```

# Wrapping C in a Haskell API

😱

```haskell
import Data.ByteString (ByteString)
import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CSize(..))

import qualified Data.Binary
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Unsafe
import qualified Foreign.Marshal.Unsafe

foreign import ccall "run" c_run
    :: Ptr CChar -> CSize -> Ptr CChar -> IO CChar

run :: StateMachine -> ByteString -> Transition
run stateMachine input = Transition f
  where
    step = Data.ByteString.Lazy.toStrict (Data.Binary.encode stateMachine)

    io =
        Data.ByteString.Unsafe.unsafeUseAsCStringLen step  $ \(ptrStep , _  ) -> do
        Data.ByteString.Unsafe.unsafeUseAsCStringLen input $ \(ptrBytes, len) -> do
        c_run ptrBytes (fromIntegral len) ptrStep

    f startingState =
      toEnum (fromEnum (Foreign.Marshal.Unsafe.unsafeLocalState io))
```

# Question

How do we parallelize `accept`? 🤔

```haskell
accept :: StateMachine input -> [input] -> Bool
accept (StateMachine {..}) startingInputs = 
    Data.Set.member finalState acceptingStates
  where
    finalState = loop startingInputs startingState

    loop [] state = state

    loop (input:inputs) state = loop inputs (step input state)
```

# Insight #1 - Use a CPU intrinsic

The paper shows that we can efficiently simulate up to 16 states at a time

... in one CPU instruction!!

GCC provides a
[`__builtin_shuffle` intrinsic](https://gcc.gnu.org/onlinedocs/gcc/Vector-Extensions.html)
for this purpose

We can understand how `__builtin_shuffle` works by analogy to Haskell code

# Restrict the number of states

Suppose that we cap the number of states to 16 (for now):

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

import Data.Binary (Binary)
import GHC.Generics (Generic)

data State = S00 | S01 | S02 | S03 | S04 | S05 | S06 | S07
           | S08 | S09 | S10 | S11 | S12 | S13 | S14 | S15
           deriving (Binary, Bounded, Enum, Eq, Generic, Ord)
```

... and hardcode our `StateMachine` type to only work on `State`s:

```haskell
data StateMachine input = StateMachine
    { startingState   :: State
    , step            :: input -> State -> State
    , acceptingStates :: Set State
    }
```

# State transitions

Now define a state transition as a function from an old `State` to a new
`State`:

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

# `__builtin_shuffle`

`__builtin_shuffle` is lets us (essentially) `mappend` `Transition`s in one CPU
instruction

```haskell
instance Monoid Transition where
    mempty = Transition id

    mappend = {-# __builtin_shuffle(_, _) #-}
```

The truth is more complicated, but we'll get there

For now, let's play code Tetris to exploit `__builtin_shuffle`

# Refactor our inner `loop`

We can refactor `accept` to use `(<>)` for the `Transition` `Monoid`

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

... and now we're in a position to exploit `__builtin_shuffle`

# The truth behind `__builtin_shuffle`

Here is how `__builtin_shuffle` *actually* works:

* Convert each `Transition` function to a "transition array"
* Marshal each transition array into C
* Call `__builtin_shuffle` to compose transition arrays
* Marshal the result back into Haskell

For efficiency, we actually marshal data to and from C in large batches

# Transition array

A transition array is a lookup table for a transition function, where:

* the index of the array represents the old state before the transition
* the value stored at that index represents the new state after the transition

For example, this transition table:

```
Old → New
─────────
S00 → S00
S01 → S04
S02 → S01
S03 → S10
S04 → S02
S05 → S00
S06 → S03
S07 → S06
S08 → S04
S09 → S12
S10 → S05
S11 → S01
S12 → S06
S13 → S04
S14 → S07
S15 → S14
```

.. corresponds to this transition array:

```haskell
> bytes transition
[0,4,1,10,2,0,3,6,4,12,5,2,6,8,7,14]
```

# Conversion

This code explains the correspondence between `Transition` functions and arrays:

```haskell
{-# LANGUAGE OverloadedLists #-}

import Data.Vector (Vector, (!))

to :: Transition -> Vector State
to (Transition f) = fmap f [minBound..maxBound]

from :: Vector State -> Transition
from states = Transition (\state -> states ! fromEnum s)
```

In practice, we convert to and from a 16-byte representation that C uses:

```haskell
instance Binary Transition where
    put (Transition f) = mapM_ (put . f) [minBound..maxBound]

    get = do
        let numStates = fromEnum (maxBound :: State) + 1
        !ss <- Data.Vector.replicateM numStates get
        return (Transition (\s -> ss ! fromEnum s))
```

# Restrict the input

There's a cost to passing transition arrays back and forth between Haskell and C

We can amortize that cost by precomputing **ALL** possible transition arrays

This requires changing our `StateMachine` type from one that accepts any input:

```haskell
data StateMachine input = StateMachine
    { startingState   :: State
    , stepFunction    :: input -> State -> State
    , acceptingStates :: Set State
    }
```

... to a `StateMachine` that only accepts bytes as input:

```haskell
import Data.Word (Word8)

data StateMachine = StateMachine
    { startingState   :: State
    , stepFunction    :: Word8 -> State -> State
    , acceptingStates :: Set State
    }
```

# Precomputing transition arrays

If we look at the type of the `stepFunction` field of `StateMachine`:

```haskell
data StateMachine = StateMachine
    { startingState   :: State
    , stepFunction    :: Word8 -> State -> State
    , acceptingStates :: Set State
    }
```

... we can encode this by precomputing `stepFunction` for all 256 possible input
bytes:

```haskell
[ minBound .. maxBound ] :: [Word8]

map stepFunction [ minBound .. maxBound ] :: [ State -> State ]

map (Transition . stepFunction) [ minBound .. maxBound ] :: [ Transition ]
```

This gives us a list of 256 `Transition`s, each of which we encode in 16 bytes

256 bytes × 16 = 4 kibibytes (compact! 😊)

# The C magic

```c
#include <stdint.h>
#include <stdlib.h>

// C representation of a transition array (16 elements, 1 byte each)
typedef uint8_t v16si __attribute__ ((vector_size (16)));

void run(char *bytes, size_t len, unsigned char *stepBytes, char *out) {
    unsigned char byte;
    int i, j;

    // Represent the step function as an array of 256 transition arrays
    v16si step[256];

    // Decode input into the format that `__builtin_shuffle` expects
    for (i = 0; i < 256; i++) {
        for (j = 0; j < 16; j++) {
            step[i][j] = stepBytes[16 * i + j];
        }
    }

    // Begin with the identity transition array
    v16si s = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };

    // Run the state machine
    for (i = 0; i < len; i ++) {
        byte = bytes[i];

        // s := step byte <> s
        s = __builtin_shuffle(step[byte], s);
    }

    // Encode output into the format that the Haskell side expects
    for (i = 0; i < 16; i++) {
        out[i] = s[i];
    }
}
```

# Haskell binding to C

Now let's wrap the C code in a purely functional API

```haskell
foreign import ccall "run" c_run
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

runSerial :: StateMachine -> ByteString -> Transition
runSerial matrix bytes = Data.Binary.decode (Data.ByteString.Lazy.fromStrict (
    Foreign.Marshal.Unsafe.unsafeLocalState (do
        Data.ByteString.Unsafe.unsafeUseAsCStringLen stepBytes (\(ptrStepBytes, _) ->
            Data.ByteString.Unsafe.unsafeUseAsCStringLen bytes (\(ptrBytes, len) ->
                Foreign.allocaBytes 16 (\ptrOut -> do
                    c_run ptrBytes (fromIntegral len) ptrStepBytes ptrOut
                    Data.ByteString.packCStringLen (ptrOut, 16) ) ) ) ) ))
  where
    stepBytes = Data.ByteString.Lazy.toStrict (Data.Binary.encode matrix)
```


# We can parallelize `mappend`

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

This won't speed things up

```haskell
let l = Transition (step i₀) <> Transition (step i₁)

    r = Transition (step i₂) <> Transition (step i₃)

in  l `par` r `par` l <> r
```

... but we can't really "evaluate" an unsaturated function call ahead of time

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
