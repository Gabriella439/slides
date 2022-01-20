% Regular expressions implemented in Haskell
% Gabriella Gonzalez
% June 22, 2016

# Goal

Here's the regular expressions API that I want:

```haskell
data Regex i = ...                  -- `i` is for "input"

match :: Regex i -> [i] -> Bool     -- A `Regex i` matches 0 or more `i`s

dot  :: Regex i                     -- Match any symbol
once :: i -> Regex i                -- Match a specific symbol

instance Num (Regex i) where
    fromInteger 0 = ...             -- Match nothing
    fromInteger 1 = ...             -- Match the empty sequence

    (+) = ...                       -- `x + y` means to match `x` or match `y`
    (*) = ...                       -- `x * y` means to match `x` then match `y`

star :: Regex i -> Regex i          -- `star x` means to match 0 or more `x`s
plus :: Regex i -> Regex i          -- `plus x` means to match 1 or more `x`s
```

... and I want it to be fast!

# High-efficiency primitives

We also need high-efficiency bulk operations

```haskell
matchBytes :: Regex Word8 -> ByteString -> Bool

bytes :: ByteString -> Regex Word8

{- Laws:

> text (x <> y) = text x * text y
> text  mempty  = 1
-}
text :: Text -> Regex Word8
text t = bytes (Data.Text.Encoding.encodeUtf8 t)

instance IsString (Regex Word8) where
    fromString s = text (Data.Text.pack s)
```

# Example use of this API

```
ab           =>  "ab"
a|b          =>  "a" + "b"
a?           =>  "a" + 1
a*           =>  star "a"
a+           =>  plus "a"
a?b          =>  ("a" + 1) * "b"               -- Same as: "ab" + "b"
a{n}         =>  "a" ^ n                       -- Why does this work?
a(b|c)*d     =>  "a" * star ("b" + "c") * "d"
```

# Haskell regular expression libraries

C backend, string API:

* `regex-posix` / `regex-pcre` / `regex-pcre-builtin` / `pcre-light` / `pcre-heavy`

Haskell backend, string API:

* `regex-parsec` / `regex-tdfa` / `regex-dfa`/`regex-pderiv`

Haskell backend, elegant Haskell API:

* `regex-applicative`

`regex-applicative` was pretty close to what I wanted

# `regex-applicative`

* Elegant mathematical API
* No catastrophic backtracking (similar to Thompson's algorithm)
* Unfortunately, about ~60x slower than `grep` for regex matching:
    * `regex-applicative`: 3 MB / s (300 ns / byte)
    * GNU `grep` (regular expression): 200 MB / s (5 ns / byte)
    * GNU `grep` (simple string search): 2 GB / s (0.5 ns / byte)

However, I wanted to see if I could get closer to `grep` in performance.

Spoiler alert:

* 50 MB / s in pure Haskell
    * ~15x faster than `regex-applicative`
    * ~4x  slower than `grep`

Teaser: Sometimes we can get 1.5 GB / s / core using a mix of Haskell and C

See: [Data-Parallel Finite-State Machines](http://research.microsoft.com/pubs/208237/asplos302-mytkowicz.pdf)

# Overview

* **Non-deterministic finite automata**
* Mathematical API
* Improving efficiency in pure Haskell
* Conclusions

# NFAs

Thompson's construction converts regular expressions to non-deterministic
finite-state automata (NFAs)

```haskell
import Data.Set (Set)

import qualified Data.Set as Set

type State = Int

-- Simple NFA
data Regex i = Regex
    { startingStates     :: Set State
    , transitionFunction :: i -> State -> Set State
    , acceptingStates    :: Set State
    }

match :: Regex i -> [i] -> Bool
match (Regex as _ bs)  []    = not (Set.null (Set.intersection as bs))
match (Regex as f bs) (i:is) = match (Regex as' f bs) is
  where
    as' = Set.unions (map (f i) (Set.elems as))
```

# Example NFA

```haskell
hello :: Regex Char
hello = Regex (Set.singleton 0) f (Set.singleton 5)
  where
    f 'H' 0 = Set.singleton 1
    f 'e' 1 = Set.singleton 2
    f 'l' 2 = Set.singleton 3
    f 'l' 3 = Set.singleton 4
    f 'o' 4 = Set.singleton 5
    f  _  _ = Set.empty
```

```haskell
>>> match hello "Hello"
True
>>> match hello "Hell"
False
>>> match hello "Hello!"
False
>>> match hello " Hello"
False
```

# Example NFA - Benchmarks

```haskell
main :: IO ()
main = print (match hello (replicate (10^8) 'A'))
```

```bash
$ bench ./Stage1
benchmarking ./Stage1
time                 1.271 s    (1.240 s .. 1.297 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.277 s    (1.269 s .. 1.281 s)
std dev              6.589 ms   (0.0 s .. 7.210 ms)
variance introduced by outliers: 19% (moderately inflated)
```

~13 ns / element (Because it simulates 0 states after 1st element)

# Interior match

```haskell
hello' :: Regex Char
hello' = Regex (Set.singleton 0) f (Set.singleton 5)
  where
    f 'H' 0 = Set.fromList [0, 1]
    f 'e' 1 = Set.fromList [0, 2]
    f 'l' 2 = Set.fromList [0, 3]
    f 'l' 3 = Set.fromList [0, 4]
    f 'o' 4 = Set.fromList [0, 5]
    f  _  5 = Set.singleton 5
    f  _  _ = Set.singleton 0
```

```bash
$ bench ./Stage2
benchmarking ./Stage2
time                 2.827 s    (2.812 s .. 2.844 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.843 s    (2.837 s .. 2.849 s)
std dev              8.789 ms   (543.9 as .. 9.418 ms)
variance introduced by outliers: 19% (moderately inflated)
```

~28 ns / element (Because it always has to simulate at least one state)

# `regex-applicative` for comparison

```haskell
import Text.Regex.Applicative

main :: IO ()
main = print (match re (replicate (10^8) 'A') /= Nothing)
  where
    re = many anySym *> string "Hello" <* many anySym
-- Note: `findFirstPrefix (string "Hello")` is *much* slower for some reason
```

```bash
$ bench ./regex-applicative
benchmarking ./RA
time                 15.05 s    (14.79 s .. 15.60 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.75 s    (14.64 s .. 14.92 s)
std dev              151.3 ms   (2.176 fs .. 166.3 ms)
variance introduced by outliers: 19% (moderately inflated)
```

151 ns / element (Only ~5x slower)

Not bad considering `regex-applicative` is actually parsing the matched element!

# More complex example

Suppose we want to match the regular expression: `.*ab*c.*`

As an NFA:

```haskell
complex :: Regex Char
complex = Regex (Set.singleton 0) f (Set.singleton 2)
  where
    f 'a' 0 = Set.fromList [0, 1]
    f 'b' 1 = Set.fromList [0, 1]
    f 'c' 1 = Set.fromList [0, 1]
    f 'd' 1 = Set.singleton 2
    f  _  2 = Set.singleton 2
    f  _  _ = Set.singleton 0
```

Using `regex-applicative`:

```haskell
many anySym *> sym 'A' *> many (sym 'C' <|> sym 'D') *> sym 'D' *> many anySym
```

# Benchmark complex example

NFA:

```bash
$ bench ./Stage3
benchmarking ./Stage3
time                 6.856 s    (6.666 s .. 7.216 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 6.791 s    (6.749 s .. 6.866 s)
std dev              64.69 ms   (0.0 s .. 67.35 ms)
variance introduced by outliers: 19% (moderately inflated)
```

`regex-applicative`:

```bash
$ bench ./RA2
benchmarking ./RA2
time                 31.75 s    (31.35 s .. 32.09 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 31.61 s    (31.50 s .. 31.68 s)
std dev              109.9 ms   (0.0 s .. 126.6 ms)
variance introduced by outliers: 19% (moderately inflated)
```

# Questions?

* Non-deterministic finite automata
* **Mathematical API**
* Improving efficiency in pure Haskell
* Conclusions

# Thompson's construction

Translate any regular expression to an NFA

See: [Regular Expression Matching Can Be Simple And Fast (but is slow in Java, Perl, PHP, Python, Ruby, ...)](https://swtch.com/~rsc/regexp/regexp1.html)

We can translate:

* `satisfy` / `once` / `dot`
* `(+)` / `0`
* `(*)` / `1`
* `star` / `plus`

# Thompson's construction - `once` / `dot`

```haskell
data Regex i = Regex
    { numberOfStates     :: Int
    -- ^ We'll need the above extra field for later
    , startingStates     :: Set State
    , transitionFunction :: i -> State -> Set State
    , acceptingStates    :: Set State
    }

satisfy :: (i -> Bool) -> Regex i
satisfy predicate = Regex n as f bs
  where
    n  = 2
    as = Set.singleton 0
    bs = Set.singleton 1

    f i 0 | predicate i = Set.singleton 1
    f _ _               = Set.empty

once :: Eq i => i -> Regex i
once x = satisfy (== x)

dot :: Regex i
dot = satisfy (\_ -> True)
```

# Examples

```haskell
>>> match (once 'A') "A"
True
>>> match (once 'A') "B"
False
>>> match (once 'A') ""
False
>>> match (once 'A') "AA"
False
```

# Thompson's construction - `0` and `1`

```haskell
instance Num (Regex i) where
    -- The regular expression that never matches anything
    fromInteger 0 = Regex n as f bs
      where
        n  = 0
        as = Set.empty
        bs = Set.empty
        f _ _ = Set.empty

    -- "ε": the regular expression that matches the empty string
    fromInteger 1 = Regex n as f bs
      where
        n  = 1
        as = Set.singleton 0
        bs = Set.singleton 0
        f _ _ = Set.empty

    ...
```

# Examples

```haskell
>>> match 0 ""
False
>>> match 0 "A"
False
>>> match 1 ""
True
>>> match 1 "A"
False
```

# Thompson's construction - `(+)`

```haskell
shift :: Int -> Set State -> Set State
shift n = Set.fromAscList . map (+ n) . Set.toAscList

instance Num (Regex i) where
    ...

    Regex nL asL fL bsL + Regex nR asR fR bsR = Regex n as f bs
      where
        n  = nL + nR
        as = Set.union asL (shift nL asR)
        bs = Set.union bsL (shift nL bsR)
        f i s | s < nL    = fL i s
              | otherwise = shift nL (fR i (s - nL))

    ...
```

# Examples

```haskell
>>> match (once 'A' + once 'B') "A"
True
>>> match (once 'A' + once 'B') "B"
True
>>> match (once 'A' + once 'B') "C"
False
>>> match (once 'A' + 1) "A"
True
>>> match (once 'A' + 1) ""
True
```

# Thompson's construction - `(*)`

```haskell
instance Num (Regex i) where
    ...

    Regex nL asL fL bsL * Regex nR asR fR bsR = Regex n as f bs
      where
        n = nL + nR

        as =
            if Set.null (Set.intersection asL bsL)
            then           asL
            else Set.union asL (shift nL asR)

        f i s =
            if s < nL
            then if Set.null (Set.intersection r bsL)
                 then           r
                 else Set.union r (shift nL asR)
            else shift nL (fR i (s - nL))
          where
            r = fL i s

        bs = shift nL bsR
```

# Examples

```haskell
>>> match (once 'A' * once 'B') "AB"
True
>>> match (once 'A' * (once 'B' + once 'C') * once 'D') "ABD"
True
>>> match (once 'A' * (once 'B' + once 'C') * once 'D') "ACD"
True
>>> match (once 'A' * (once 'B' + once 'C') * once 'D') "AD"
False
```

# Thompson's construction - `star` / `plus`

```haskell
star :: Regex i -> Regex i
star (Regex n as f bs) = Regex n as f' as
  where
    f' i s =
        let r = f i s
        in  if Set.null (Set.intersection r bs)
            then           r
            else Set.union r as

plus :: Regex i -> Regex i
plus (Regex n as f bs) = Regex n as f' bs
  where
    f' i s =
        let r = f i s
        in  if Set.null (Set.intersection r bs)
            then           r
            else Set.union r as
```

# Examples

```haskell
>>> match (star (once 'A')) "AAA"
True
>>> match (star (once 'A')) ""
True
>>> match (plus (once 'A' + once 'B')) "ABBA"
True
>>> match (plus (once 'A' + once 'B')) ""
False
```

# Benchmarks

```haskell
complex :: Regex Char
complex = star dot * once 'A' * star (once 'B' + once 'C') * once 'D' * star dot

main :: IO ()
main = print (match complex (replicate (10^8) 'A'))
```

```bash
$ time ./Stage4
False

real	3m16.912s
user	3m15.747s
sys	0m1.039s
````

3.2 us / element (**E_TOO_MANY_SET_OPERATIONS**)

Can we keep the elegant mathematical approach but still be efficient?

# Questions?

* Non-deterministic finite automata
* Mathematical API
* **Improving efficiency in pure Haskell**
* Conclusions

# Optimizing `Set` operations

There's a much more efficient representation for `Set Int`: an `Integer`!

* Each state is a bit in the `Integer`
* 0th bit is state #0, 1st bit is state #1, etc.
* If bit #N is 0, then state #N is unoccupied
* If bit #N is 1, then state #N is occupied

Example:

```
19 (decimal) => 10011 (binary) => States #0,#1,#4 occupied
0  (decimal) =>     0 (binary) => All states unoccupied
31 (decimal) => 11111 (binary) => States #0,#1,#2,#3,#4,#5 occuppied
```

`Integer`s are unlimited precision, so we can hold any number of states

# Bitwise operations

We can translate slower `Data.Set` functions into faster `Data.Bits` operations:

```haskell
  Data.Set         Data.Bits
____________      ___________
Set Int       =>  Integer
empty         =>  0
singleton     =>  shiftL 1
intersection  =>  (.&.)
union         =>  (.|.)
shift         =>  flip shiftL
null          =>  (== 0)
```

```haskell
data Regex i = Regex
    { numberOfStates     :: Int
    , startingStates     :: Integer
    , transitionFunction :: i -> Int -> Integer
    , acceptingStates    :: Integer
    }
```

# Benchmark

Using `Set`-based operations:

```haskell
$ time ./Stage4
False

real	3m16.912s
user	3m15.747s
sys	0m1.039s
```

Using `Integer` bitwise operations:

```haskell
$ time ./Stage5
False

real	0m38.898s
user	0m38.770s
sys	0m0.103s
```

**~5x faster**, but still slower than `regex-applicative`

# `Word` instead of `Integer` 

`Word`s are faster than `Integer` for bit operations, but only hold 64 states

```haskell
data Regex i = Regex
    { _numberOfStates         :: Int

    , startingStates         :: Word
    , transitionFunction     :: i -> Int -> Word
    , acceptingStates        :: Word

    , startingStatesSlow     :: Integer
    , transitionFunctionSlow :: i -> Int -> Integer
    , acceptingStatesSlow    :: Integer
    }

match :: Regex i -> [i] -> Bool
match (Regex n as f bs cs g ds) is
    | n <= sizeOfWord = ... {- Use faster `Word` as our bit set -}
    | otherwise       = ... {- Use slower `Integer` as our bit set -}
  where
    sizeOfWord = Bits.finiteBitSize (0 :: Word)
```

... so let's add a fast path for 64 or fewer states!

# Benchmarks

Using `Integer` bitwise operations:

```haskell
$ time ./Stage5
False

real	0m38.898s
user	0m38.770s
sys	0m0.103s
```

Using `Word` bitwise operations:

```bash
$ bench ./Stage6
benchmarking ./Stage6
time                 4.913 s    (4.893 s .. 4.956 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.934 s    (4.926 s .. 4.939 s)
std dev              8.211 ms   (0.0 s .. 9.470 ms)
variance introduced by outliers: 19% (moderately inflated)
```

**~8x faster**, but still slower than `grep`

# `ByteString`-specific optimizations

* Use a precomputed lookup table for each state transition

* Add matching functions specialized to `ByteString`s for tight loops

    ```haskell
    hasBytes :: Regex Word8 -> ByteString -> Bool
    hasBytes (Regex n as f bs cs g ds) (ByteString.Internal.PS fp off len)
        | n <= sizeOfWord = do
            ByteString.Internal.accursedUnutterablePerformIO
                (withForeignPtr fp (\p ->
                    loop as (p `plusPtr` off) (p `plusPtr` (off+len)) ))
        ...
      where
        loop !z !p !q
            | bs .&. z /= 0 = return True
            | p == q        = return False
            | otherwise     = do
                x <- peek p
                loop (step z x .|. as) (p `plusPtr` 1) q
        ...
    ```

* `mmap` `ByteString` when reading from a file

# Benchmarks

Before:

```haskell
$ bench ./Stage6
benchmarking ./Stage6
time                 4.913 s    (4.893 s .. 4.956 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.934 s    (4.926 s .. 4.939 s)
std dev              8.211 ms   (0.0 s .. 9.470 ms)
variance introduced by outliers: 19% (moderately inflated)
```

After:

```haskell
$ bench ./Stage7
benchmarking ./Stage7
time                 1.824 s    (1.755 s .. 1.884 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.807 s    (1.795 s .. 1.813 s)
std dev              10.42 ms   (0.0 s .. 11.55 ms)
variance introduced by outliers: 19% (moderately inflated)
```

**~3x speed improvement**

# Comparison to `grep`

Pure Haskell implementation:

```haskell
$ bench ./Stage7
benchmarking ./Stage7
time                 1.824 s    (1.755 s .. 1.884 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.807 s    (1.795 s .. 1.813 s)
std dev              10.42 ms   (0.0 s .. 11.55 ms)
variance introduced by outliers: 19% (moderately inflated)
```

`grep` on same regular expression and input file:

```haskell
benchmarking ./Stage7
time                 448.5 ms   (408.9 ms .. 477.5 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 430.0 ms   (431.8 ms .. 442.8 ms)
std dev              10.42 ms   (0.0 s .. 11.55 ms)
variance introduced by outliers: 19% (moderately inflated)
```

Only ~4x slower than `grep`

# Final code

```haskell
{-# LANGUAGE BangPatterns #-}

module Regex where

import Data.Array.Unboxed (Array, UArray, (!))
import Data.Bits ((.|.), (.&.))
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Word (Word8, Word64)
import Foreign (peek, plusPtr, withForeignPtr)

import qualified Data.Array.Base
import qualified Data.Array.Unboxed       as Array
import qualified Data.Bits                as Bits
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Internal as ByteString.Internal
import qualified GHC.Arr

integerShiftL :: Integer -> Int -> Integer
integerShiftL = Bits.unsafeShiftL
{-# INLINE integerShiftL #-}

sizeOfWord :: Int
sizeOfWord = Bits.finiteBitSize (0 :: Word)
{-# INLINE sizeOfWord #-}

integerFoldl' :: (b -> Int -> b) -> b -> Integer -> b
integerFoldl' f acc0 bits = go acc0 (Bits.popCount bits) 0
  where
    go !acc 0 _ = acc
    go !acc n b =
        if Bits.testBit bits b
        then go (f acc b) (n - 1) (b + 1)
        else go    acc     n      (b + 1)
{-# INLINE integerFoldl' #-}

data Regex i = Regex
    { _numberOfStates         :: Int

    -- Fast path, if the number of states is less than or equal to the number of
    -- bits in a `Word`
    , _startingStates         :: Word
    , _transitionFunction     :: i -> Int -> Word
    , _acceptingStates        :: Word

    -- Slow path, if the number of states is greater than the number of bits in
    -- a `Word`
    --
    -- This is ~10x slower
    , _startingStatesSlow     :: Integer
    , _transitionFunctionSlow :: i -> Int -> Integer
    , _acceptingStatesSlow    :: Integer
    }

instance Num (Regex i) where
    fromInteger n
        | 0 == n    = Regex 0 0 f 0 0 g 0
        | 0 <  n    = Regex 1 1 f 1 1 g 1
        | otherwise = error "fromInteger[Regex]: Negative numbers unsupported"
      where
        f _ _ = 0
        g _ _ = 0
    {-# INLINE fromInteger #-}

    Regex nL asL fL bsL csL gL dsL + Regex nR asR fR bsR csR gR dsR =
        Regex n as f bs cs g ds
      where
        n  = nL + nR

        as = Bits.unsafeShiftL asR nL .|. asL

        f i j =
            if j < nL
            then fL i j
            else Bits.unsafeShiftL (fR i (j - nL)) nL

        bs = Bits.unsafeShiftL bsR nL .|. bsL

        cs = integerShiftL csR nL .|. csL

        g i j =
            if j < nL
            then gL i j
            else integerShiftL (gR i (j - nL)) nL

        ds = integerShiftL dsR nL .|. dsL
    {-# INLINE (+) #-}

    Regex nL asL fL bsL csL gL dsL * Regex nR asR fR bsR csR gR dsR =
        asR' `seq` csR' `seq` Regex n as f bs cs g ds
      where
        n = nL + nR

        asR' = Bits.unsafeShiftL asR nL

        as =
            if asL .&. bsL == 0
            then asL
            else asL .|. asR'

        f i j =
            if j < nL
            then
                if s .&. bsL == 0
                then s
                else s .|. asR'
            else Bits.unsafeShiftL (fR i (j - nL)) nL
          where
            s = fL i j

        bs = Bits.unsafeShiftL bsR nL

        csR' = integerShiftL csR nL

        cs =
            if csL .&. dsL == 0
            then csL
            else csR' .|. csL

        g i j =
            if j < nL
            then
                if s .&. dsL == 0
                then s
                else s .|. csR'
            else integerShiftL (gR i (j - nL)) nL
          where
            s = gL i j

        ds = integerShiftL dsR nL
    {-# INLINE (*) #-}

star :: Regex i -> Regex i
star (Regex n as f bs cs g ds) = Regex n as f' as cs g' cs
  where
    f' i j =
        let s = f i j
        in  if s .&. bs == 0
            then s
            else s .|. as

    g' i j =
        let s = g i j
        in  if s .&. ds == 0
            then s
            else s .|. cs
{-# INLINE star #-}

plus :: Regex i -> Regex i
plus (Regex n as f bs cs g ds) = Regex n as f' bs cs g' ds
  where
    f' i j =
        let s = f i j
        in  if s .&. bs == 0
            then s
            else s .|. as

    g' i j =
        let s = g i j
        in  if s .&. ds == 0
            then s
            else s .|. cs
{-# INLINE plus #-}

match :: Regex i -> [i] -> Bool
match (Regex n as f bs cs g ds) is
    -- Fast path (Bit arithmetic on `Word`s)
    | n <= sizeOfWord = bs .&. foldl' step  as is /= 0
    -- Slow path (Bit arithmetic on `Integer`s)
    | otherwise       = ds .&. foldl' step' cs is /= 0
  where
    step s0 i = go 0 s0
      where
        go !acc 0 = acc
        go !acc s = go (acc .|. f i j) (Bits.clearBit s j)
          where
            j = Bits.countTrailingZeros s

    step' s0 i = integerFoldl' (\acc j -> acc .|. g i j) 0 s0
{-# INLINE match #-}

satisfy :: (i -> Bool) -> Regex i
satisfy predicate = Regex 2 1 f 2 1 g 2
  where
    f c 0 | predicate c = 2
    f _ _               = 0

    g c 0 | predicate c = 2
    g _ _               = 0
{-# INLINE satisfy #-}

once :: Eq i => i -> Regex i
once x = satisfy (== x)
{-# INLINE once #-}

dot :: Regex i
dot = satisfy (\_ -> True)
{-# INLINE dot #-}

chars :: Regex i
chars = Regex 1 1 f 1 1 g 1
  where
    f _ _ = 1
    g _ _ = 1
{-# INLINE chars #-}

bytes :: ByteString -> Regex Word8
bytes w8s = Regex (n + 1) 1 f (Bits.unsafeShiftL 1 n) 1 g (integerShiftL 1 n)
  where
    n = fromIntegral (ByteString.length w8s)

    f w8 i
        | i == n                                      =
            0
        | ByteString.index w8s (fromIntegral i) == w8 =
            Bits.unsafeShiftL 1 (i + 1)
        | otherwise                                   =
            0

    g w8 i
        | i == n                                      =
            0
        | ByteString.index w8s (fromIntegral i) == w8 =
            integerShiftL 1 (i + 1)
        | otherwise                                   =
            0

matchBytes :: Regex Word8 -> ByteString -> Bool
matchBytes (Regex n as f bs cs g ds) (ByteString.Internal.PS fp off len)
    | n <= sizeOfWord = do
        ByteString.Internal.accursedUnutterablePerformIO
            (withForeignPtr fp (\p ->
                loop as (p `plusPtr` off) (p `plusPtr` (off+len)) ))
    | otherwise       = do
        ByteString.Internal.accursedUnutterablePerformIO
            (withForeignPtr fp (\p ->
                loop' cs (p `plusPtr` off) (p `plusPtr` (off+len)) ))
  where
    loop  0  _  _   = return False
    loop !z !p !q
        | p == q    = return (bs .&. z /= 0)
        | otherwise = do
            x <- peek p
            loop (step z x) (p `plusPtr` 1) q

    step :: Word -> Word8 -> Word
    step !s0 i0 = go 0 s0
      where
        go :: Word -> Word -> Word
        go !acc 0 = acc
        go !acc s = go acc' s'
          where
            acc' = acc .|. m
            m    = Data.Array.Base.unsafeAt table ix
            ix   = GHC.Arr.unsafeIndex bounds (i0, j)
            s'   = s .&. Bits.complement (Bits.unsafeShiftL 1 j)
            j    = Bits.countTrailingZeros s

        bounds :: ((Word8, Int), (Word8, Int))
        bounds = ((0, 0), (255, n - 1))

        table :: UArray (Word8, Int) Word
        table =
            Array.listArray bounds
                [ f i j
                | i <- [0..255]
                , j <- [0..n-1]
                ]

    loop'  0  _  _  = return False
    loop' !z !p !q
        | p == q    = return (ds .&. z /= 0)
        | otherwise = do
            x <- peek p
            loop' (step' z x) (p `plusPtr` 1) q

    step' :: Integer -> Word8 -> Integer
    step' !s0 i0 = integerFoldl' (\acc j -> acc .|. table ! (i0, j)) 0 s0
      where
        table :: Array (Word8, Int) Integer
        table =
            Array.listArray ((0, 0), (255, n - 1))
                [ g i j
                | i <- [0..255]
                , j <- [0..n-1]
                ]

hasBytes :: Regex Word8 -> ByteString -> Bool
hasBytes (Regex n as f bs cs g ds) (ByteString.Internal.PS fp off len)
    | n <= sizeOfWord = do
        ByteString.Internal.accursedUnutterablePerformIO
            (withForeignPtr fp (\p ->
                loop as (p `plusPtr` off) (p `plusPtr` (off+len)) ))
    | otherwise       = do
        ByteString.Internal.accursedUnutterablePerformIO
            (withForeignPtr fp (\p ->
                loop' cs (p `plusPtr` off) (p `plusPtr` (off+len)) ))
  where
    loop !z !p !q
        | bs .&. z /= 0 = return True
        | p == q        = return False
        | otherwise     = do
            x <- peek p
            loop (step z x .|. as) (p `plusPtr` 1) q

    step :: Word -> Word8 -> Word
    step !s0 i0 = go 0 s0
      where
        go :: Word -> Word -> Word
        go !acc 0 = acc
        go !acc s = go acc' s'
          where
            acc' = acc .|. m
            m    = Data.Array.Base.unsafeAt table ix
            ix   = GHC.Arr.unsafeIndex bounds (i0, j)
            s'   = s .&. Bits.complement (Bits.unsafeShiftL 1 j)
            j    = Bits.countTrailingZeros s

        bounds :: ((Word8, Int), (Word8, Int))
        bounds = ((0, 0), (255, n - 1))

        table :: UArray (Word8, Int) Word
        table =
            Array.listArray bounds
                [ f i j
                | i <- [0..255]
                , j <- [0..n-1]
                ]

    loop' !z !p !q
        | ds .&. z /= 0 = return True
        | p == q        = return False
        | otherwise     = do
            x <- peek p
            loop' (step' z x .|. cs) (p `plusPtr` 1) q

    step' :: Integer -> Word8 -> Integer
    step' !s0 i0 = integerFoldl' (\acc j -> acc .|. table ! (i0, j)) 0 s0
      where
        table :: Array (Word8, Int) Integer
        table =
            Array.listArray ((0, 0), (255, n - 1))
                [ g i j
                | i <- [0..255]
                , j <- [0..n-1]
                ]
```

# Command-line utility

I also wrapped the above code in a convenient command-line utility:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8)
import Regex
import Text.Trifecta (Parser, Result(..))
import Text.Parser.Combinators (eof, try)
import Text.Parser.Token (charLiteral, stringLiteral, textSymbol)

import qualified Control.Parallel.Strategies as Parallel
import qualified Data.ByteString             as ByteString
import qualified Data.ByteString.Char8       as ByteString.Char8
import qualified Data.ByteString.Search      as ByteString.Search
import qualified Data.List.Split
import qualified Data.Text.Encoding          as Text.Encoding
import qualified Options.Generic
import qualified System.IO.MMap
import qualified Text.Trifecta

data AST
    = Zero
    | One
    | Dot
    | Add AST AST
    | Times AST AST
    | Star AST
    | Plus AST
    | Bytes ByteString

parseExpr0 :: Parser AST
parseExpr0
    =   try (Add <$> parseExpr1 <*> (textSymbol "+" *> parseExpr0))
    <|> parseExpr1

parseExpr1 :: Parser AST
parseExpr1
    =   try (Times <$> parseExpr2 <*> (textSymbol "*" *> parseExpr1))
    <|> parseExpr2

parseExpr2 :: Parser AST
parseExpr2
    =   Star <$> (textSymbol "star" *> parseExpr3)
    <|> Plus <$> (textSymbol "plus" *> parseExpr3)
    <|> parseExpr3

parseExpr3 :: Parser AST
parseExpr3
    =   (textSymbol "0"   *> pure Zero)
    <|> (textSymbol "1"   *> pure One )
    <|> (textSymbol "dot" *> pure Dot )
    <|> (Bytes . Text.Encoding.encodeUtf8 <$> stringLiteral)
    <|> (textSymbol "(" *> parseExpr0 <* textSymbol ")")

interpret :: AST -> Regex Word8
interpret  Zero       = 0
interpret  One        = 1
interpret  Dot        = dot
interpret (Add   x y) = interpret x + interpret y
interpret (Times x y) = interpret x * interpret y
interpret (Star  x  ) = star (interpret x)
interpret (Plus  x  ) = plus (interpret x)
interpret (Bytes x  ) = bytes x

main :: IO ()
main = do
    (str, path) <- Options.Generic.getRecord "Regular expression program"
    bytes       <- System.IO.MMap.mmapFileByteString path Nothing
    case Text.Trifecta.parseString (parseExpr0 <* eof) mempty str of
        Failure err -> print err
        Success ast -> do
            let transform =
                      ByteString.Char8.unlines 
                    . filter predicate
                    . ByteString.Char8.lines
            ByteString.putStr (transform bytes)
```

# Example usage

This utility parses a subset of Haskell and behaves like simple `grep`:

```bash
$ ./haskell-grep '"A" * star ("B" + "C") + "D"' file.txt
...
```

`grep` still way faster due to smart handling of files with lots of small lines

# Questions?

* Non-deterministic finite automata
* Mathematical API
* Improving efficiency in pure Haskell
* **Conclusions**

# Conclusions

* Typed, elegant, and efficient: pick 3!
* I'll eventually polish and release this code as a Haskell library
* Teaser: For more restricted state machines we can go **much** faster

I've included additional slides for the teaser

# We can beat `grep`!

We have the technology: [Data-Parallel Finite-State Machines](http://research.microsoft.com/pubs/208237/asplos302-mytkowicz.pdf)

I've only implemented a subset of the paper:

* Only string matching
* Maximum of 16 states

The paper explains how to support more complex operations with more states

I will gloss over a lot, so read the paper!

# Gist of the paper

You can efficiently run a finite state machine in parallel

If your machine has N states:

* Split your input sequence into K chunks
* Simulate all N starting states for each chunk to build a transition matrix
* Combine all K transition matrices into a single transition matrix at the end

The hard part is making this efficient when N is greater than the number of core
s

# High-performance state transitions

**Insight #1:** You can simulate 16 states in a single CPU instruction

Compile this with: `gcc -march=native -O3` (`clang` won't work)

```c
#include <stdint.h>
#include <stdlib.h>

typedef uint8_t v16si __attribute__ ((vector_size (16)));

void process(char *in, size_t len, unsigned char *tBytes, char *out) {
    unsigned char a;
    int i, j;
    // Initialize 16 starting states
    v16si s = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    v16si t[256];

    // Read in the state machine from Haskell land
    for (i = 0; i < 256; i++) {
        for (j = 0; j < 16; j++) {
            t[i][j] = tBytes[16 * i + j];
        }
    }

    // Loop over each byte
    for (i = 0; i < len; i++) {
        a = in[i];
        s = __builtin_shuffle(t[a], s);  // Simulate 16 states in 1 instruction
    }

    // Write out the computed transition matrix back to Haskell land
    for (i = 0; i < 16; i++) {
        out[i] = s[i];
    }
}
```

# Coarse-grained parallelism

**Insight #2**: The transition matrix is a `Monoid`, so embarassingly parallel

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Foreign (Ptr)
import Foreign.C.Types

import qualified Control.Concurrent
import qualified Control.Parallel.Strategies
import qualified Data.ByteString
import qualified Data.ByteString.Unsafe
import qualified Foreign
import qualified Foreign.Marshal.Unsafe
import qualified System.IO.MMap

foreign import ccall "process" c_process
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

-- Wrap C `process` in a pure interface
process :: ByteString -> ByteString
process bytes =
    Foreign.Marshal.Unsafe.unsafeLocalState (do
        Data.ByteString.Unsafe.unsafeUseAsCStringLen tBytes (\(ptrTBytes, _) ->
            Data.ByteString.Unsafe.unsafeUseAsCStringLen bytes (\(ptrIn, len) ->
                Foreign.allocaBytes 16 (\ptrOut -> do
                    c_process ptrIn (fromIntegral len) ptrTBytes ptrOut
                    Data.ByteString.packCStringLen (ptrOut, 16) ) ) ) )

-- Example transition matrix from paper for matching C-style comments
tBytes :: ByteString
tBytes =
    Data.ByteString.pack
        (concat
            (   replicate 42 def
            ++  [t42]
            ++  replicate 4 def
            ++  [t47]
            ++  replicate 208 def
            )
        )
  where
    def = [0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    t42 = [0, 2, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    t47 = [1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

-- | Split a `ByteString` into chunks of size @n@
chunkBytes :: Int -> ByteString -> [ByteString]
chunkBytes n bytes =
    if Data.ByteString.null bytes
    then []
    else prefix : chunkBytes n suffix
  where
    ~(prefix, suffix) = Data.ByteString.splitAt n bytes

-- Split the `ByteString` into @k@ chunks and call `process` in parallel
parallelProcess :: Int -> ByteString -> [ByteString]
parallelProcess k bytes =
    Control.Parallel.Strategies.parMap
        Control.Parallel.Strategies.rseq
        process
        (chunkBytes (len `div` k) bytes)
  where
    len = Data.ByteString.length bytes

main :: IO ()
main = do
    k     <- Control.Concurrent.getNumCapabilities
    bytes <- System.IO.MMap.mmapFileByteString "test.txt" Nothing
    -- Not yet implemented, combine the `k` transition matrices into the final
    -- transition matrix.  This is cheap since `k` is the number of cores, I
    -- just haven't had a chance to complete this yet.
    print (parallelProcess k bytes)
```

# Fine-grained parallelism

**Insight #3**: The same principle can be used for instruction-level parallelism

```c
#include <stdint.h>
#include <stdlib.h>

typedef uint8_t v16si __attribute__ ((vector_size (16)));

void process(char *in, size_t len, unsigned char *tBytes, char *out) {
    unsigned char a, b, c, d, e, f, g;
    int i, j;
    v16si s = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    v16si s0, s1, s2, s3, s4, s5;
    v16si t[256];

    for (i = 0; i < 256; i++) {
        for (j = 0; j < 16; j++) {
            t[i][j] = tBytes[16 * i + j];
        }
    }

    for (i = 0; i + 6 < len; i += 7) {
        // These can be run in parallel
        a = in[i    ];
        b = in[i + 1];
        c = in[i + 2];
        d = in[i + 3];
        e = in[i + 4];
        f = in[i + 5];
        g = in[i + 6];

        // These can be run in parallel
        s0 = __builtin_shuffle(t[a], s   );
        s1 = __builtin_shuffle(t[c], t[b]);
        s2 = __builtin_shuffle(t[e], t[d]);
        s3 = __builtin_shuffle(t[g], t[f]);

        // These can be run in parallel
        s4 = __builtin_shuffle(s1  , s0  );
        s5 = __builtin_shuffle(s3  , s2  );

        s  = __builtin_shuffle(s5  , s4  );
    }
    for (j = i; j < len; j++) {
        a = in[j];
        s = __builtin_shuffle(t[a], s);
    }

    for (i = 0; i < 16; i++) {
        out[i] = s[i];
    }
}
```

# Adhoc build instructions

Because I haven't had a chance to turn this into a proper Haskell project:

```bash
$ gcc -O3 -march=native -c process.c -o process.o
$ stack build bytestring parallel mmap
$ stack ghc -- -threaded -with-rtsopts=-N -O2 process.hs process.o
$ # Create a 1 GB file named "test.txt"
$ bench ./process  # My machine has 2 cores
benchmarking ./process
time                 318.5 ms   (315.2 ms .. 320.7 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 317.3 ms   (316.5 ms .. 317.9 ms)
std dev              878.6 μs   (475.1 μs .. 1.221 ms)
variance introduced by outliers: 16% (moderately inflated)
```

Voila!  3+ GB / s on 2 cores for any state machine with <= 16 states

# LICENSE

All code in these slides is released under a BSD 3-clause license

```
Copyright Gabriella Gonzalez 2021

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
