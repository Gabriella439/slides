% Liquid Haskell
% Gabriel Gonzalez
% May 6, 2016

# Introduction

Liquid Haskell is a backwards-compatible extension to Haskell's type system

You can use Liquid Haskell to add preconditions:

```haskell
{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x
```

... or use Liquid Haskell to check postconditions:

```haskell
{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then negate x else x
```

# The golden rule of programming

**Prefer pushing requirements upstream over pushing problems downstream**

Example:

```haskell
head :: [a] -> a
```

Pushing the problem downstream:

```haskell
head :: [a] -> Maybe a
```

Pushing the requirement upstream:

```haskell
{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
```

# Getting started

Create this `stack.yaml` file:

```yaml
resolver: lts-5.15
packages: []
extra-deps:
- daemons-0.2.1
- liquid-fixpoint-0.5.0.1
- located-base-0.1.0.0
- dotgen-0.4.2
- fgl-visualize-0.1.0.1
- intern-0.9.1.4
- aeson-0.11.2.0
```

... then run:

```bash
$ stack install liquidhaskell
```

Now the `liquid` executable will be on your `$PATH`

# How did I create that `stack.yaml` file?

I began with this `stack.yaml` file:

```yaml
resolver: lts-5.15
packages: []
```

... then just kept running:

```bash
$ stack install liquidhaskell
```

Each time I follow `stack`'s recommendation for how to fix the file.

# Test drive

Create the following file:

```haskell
$ cat example.hs
import Prelude hiding (head, abs)

{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x

{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then negate x else x
```

... then type-check the file:

```haskell
$ liquid --totality example.hs
...
**** RESULT: SAFE **************************************************************
```

Now try to break it!

# Overview

* Part 1 - Liquid Haskell 101
* Part 2 - High-performance parsing
* Part 3 - Succinct vectors

# Questions?

* Next: Liquid Haskell 101

# Types + Predicates

Liquid Haskell lets you decorate any type with a predicate.

If you decorate function inputs you get preconditions:

```haskell
{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
```

If you decorate function outputs you get postconditions:

```haskell
{-@ abs :: Int -> { n : Int | 0 <= n } @-}
```

# Preconditions vs Postconditions

Adding preconditions is free

Adding postconditions is not free.  You have to either ...

* Prove the postcondition from some precondition:

```haskell
example :: { x : Int | 0 <= x } -> { x : Int | 0 <= x }
example x = x
```

* Cheat and `assume` the postcondition

```haskell
{-@ assume (<) :: x : Int -> y : Int -> { b : Bool | Prop b <=> x < y } @-}
```

* Prove the postcondition from another postcondition

```haskell
{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then negate x else x
```

# More complex example

```haskell
{-@
mySplitAt
    :: n : { n : Int | 0 <= n }
    -> xs : { xs : [a] | n <= len xs }
    -> ({ ls : [a] | len ls == n }, { rs : [a] | len rs = len xs - n })
@-}
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0  xs    = (  [], xs)
mySplitAt n (x:xs) = (x:ls, rs)
  where
    ~(ls, rs) = mySplitAt (n - 1) xs
```

Liquid Haskell verifies that the non-exhaustive pattern match is safe!

# Promotion

You can promoted a restricted subset of Haskell to the type level

```haskell
{-@ measure myLength @-}
myLength :: [a] -> Int
myLength  []    = 0
myLength (x:xs) = 1 + myLength xs

{-@ myHead :: { xs : [a] | 1 <= myLength xs } -> a @-}
myHead :: [a] -> a
myHead (x:_) = x
```

# Subtyping

```haskell
{-@ zero :: { n : Int | n == 0 } @-}
zero :: Int
zero = 0

{-@ zero :: { n : Int | 0 <= n } @-}
zero' :: Int
zero' = zero

{-@ zero :: { n : Int | n mod 2 == 0 } @-}
zero'' :: Int
zero'' = zero
```

# Set logic

Liquid Haskell also supports type-level set operations

```haskell
import Data.Set ()  -- Import `Set_mem` and `listElts` measures

{-@
find
    :: f  : (a -> Bool)
    -> xs : [a]
    -> Maybe { x : a | Set_mem x (listElts xs) }
@-}
find :: (a -> Bool) -> [a] -> Maybe a
find predicate  [] = Nothing
find predicate (x:xs)
    | predicate x = Just x
    | otherwise   = find predicate xs
```

# Refined `delete`

There's a bug in this code!  See if you can find and fix it:

```haskell
import Data.Set ()

{-@ 
delete :: Eq a => x : a -> [a] -> { zs : [a] | not (Set_mem x (listElts zs)) }
@-}
delete :: Eq a => a -> [a] -> [a]
delete _  []    = []
delete x (y:ys)
    | x == y    =   delete x ys
    | otherwise = x:delete x ys
```

# Totality checker

Liquid Haskell rejects this program:

```haskell
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)
```

How do we fix it?

# Customization

You can teach Liquid Haskell new tricks, by creating your own "measure"s:

```haskell
{-@ measure bslen :: ByteString -> Int @-}
```

... and using those "measure"s as preconditions and postconditions

```haskell
{-@ assume length :: bs : ByteString -> { n : Int | n == bslen bs } @-}
length :: ByteString -> Int

{-@ head :: { bs : ByteString | 1 <= bslen bs } -> Word8 @-}
head :: ByteString -> Word8
```

# Example code - Runtime check

```haskell
import Data.ByteString (ByteString)
import Data.Word (Word8)

import qualified Data.ByteString as ByteString

{-@ measure bslen :: ByteString -> Int @-}

{-@ assume ByteString.length :: bs : ByteString -> { n : Int | n == bslen bs } @-}

{-@ ByteString.head :: { bs : ByteString | 1 <= bslen bs } -> Word8 @-}

runtimeCheck :: ByteString -> Maybe Word8
runtimeCheck bytes =
    if 1 <= ByteString.length bytes
    then Just (ByteString.head bytes)
    else Nothing
```

# Example code - Static check

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)

import qualified Data.ByteString as ByteString

{-@ measure bslen :: ByteString -> Int @-}

{-@ assume ByteString.pack :: [Char] -> { bs : ByteString | bslen bs == len w8s } @-}

{-@ ByteString.head :: { bs : ByteString | 1 <= bslen bs } -> Char @-}

staticCheck :: Char
staticCheck = head "Hello, world!"
```

# Type-annotated source code

When you type-check a file you get HTML source showing inferred types:

[Click to view example](.liquid/example.hs.html)

# Learning Liquid Haskell

* 20% is actually learning how to prove things in Liquid Haskell
    * This is pretty easy because Liquid Haskell does most of the work for you
* 40% is reporting bugs that you encounter 
    * The maintainers are very responsive and actively fix reported bugs
* 40% is building a mental model of what Liquid Haskell can and cannot do
    * This is extra hard because Liquid Haskell continues to grow more powerful

# Practical applications

Liquid Haskell's killer use case is statically catching out-of-bounds errors

* Out-of-the-box support from the "Liquid Haskell Prelude"
* Steers clear of sharp corners in the compiler
* Practically infeasible within any other tool (in Haskell)

Most other alternatives are either:

* Not static (i.e. runtime exceptions)
* Inefficient (i.e. don't work on `Vector`s or `ByteString`s)
* Brittle (i.e. can't scale because the type checker can't solve arithmetic)
* Unreadable (i.e. types are hairy and you have to butcher your code)

# Questions?

* Next: High-performance parsing

# Packet header parsing

At Awake Networks we use Haskell for high-performance protocol parsing and
analysis

`binary` and `cereal` are sometimes not quite fast enough for our purposes!

# Simplified parser

```haskell
import Prelude hiding (take)
import Data.ByteString (ByteString)

import qualified Data.ByteString as ByteString

-- NOTE: Using `newtype` triggers a Liquid Haskell bug!  *sigh*
-- I use `newtype` when examining core and performing benchmarks
-- I use `data` when type-checking the code using Liquid Haskell
data Parser a = Parser { runParser :: ByteString -> Maybe (a, ByteString) }

take :: Int -> Parser ByteString
take n = Parser (\bs ->
    if ByteString.length bs < n
    then Nothing
    else Just (ByteString.splitAt n bs) )

instance Monad Parser where
    return x = Parser (\bs -> Just (x, bs))

    m >>= f = Parser (\bs0 -> do
        (x, bs1) <- runParser m bs0
        runParser (f x) bs1 )
```

# UDP packet header

```haskell
{-# LANGUAGE RecordWildCards #-}

-- We could totally use a better type than this, but humor me
data UDP = UDP
    { udpSourcePort      :: !ByteString  -- 2 bytes
    , udpDestinationPort :: !ByteString  -- 2 bytes
    , udpLength          :: !ByteString  -- 2 bytes
    , udpChecksum        :: !ByteString  -- 2 bytes
    }

udp :: Parser UDP
udp = do
    udpSourcePort      <- take 2
    udpDestinationPort <- take 2
    udpLength          <- take 2
    udpChecksum        <- take 2
    return (UDP {..})
```

We're wastefully checking the length of the `ByteString` four times!

We're binding a `Maybe` result four times

# GHC Core

```haskell
$w$j
$w$j =
  \ w_sIA ww_sID ww1_sIE ww2_sIF ww3_sIG ->
    case <# ww3_sIG 2 of _ {
      False ->
        let {
          $w$j1_sIV
          $w$j1_sIV =
            \ w1_sIp ww4_sIs ww5_sIt ww6_sIu ww7_sIv ->
              case <# ww7_sIv 2 of _ {
                False ->
                  case >=# 2 ww7_sIv of _ {
                    False ->
                      let {
                        ww8_XJT
                        ww8_XJT = -# ww7_sIv 2 } in
                      case <# ww8_XJT 2 of _ {
                        False ->
                          let {
                            ww9_XJQ
                            ww9_XJQ = +# ww6_sIu 2 } in
                          case >=# 2 ww8_XJT of _ {
                            False ->
                              Just
                                (UDP
                                   w_sIA
                                   w1_sIp
                                   (PS ww4_sIs ww5_sIt ww6_sIu 2)
                                   (PS ww4_sIs ww5_sIt ww9_XJQ 2),
                                 PS ww4_sIs ww5_sIt (+# ww9_XJQ 2) (-# ww8_XJT 2));
                            True ->
                              Just
                                (UDP
                                   w_sIA
                                   w1_sIp
                                   (PS ww4_sIs ww5_sIt ww6_sIu 2)
                                   (PS ww4_sIs ww5_sIt ww9_XJQ ww8_XJT),
                                 empty)
                          };
                        True -> Nothing
                      };
                    True -> Nothing
                  };
                True -> Nothing
              } } in
        case >=# 2 ww3_sIG of _ {
          False ->
            $w$j1_sIV
              (PS ww_sID ww1_sIE ww2_sIF 2)
              ww_sID
              ww1_sIE
              (+# ww2_sIF 2)
              (-# ww3_sIG 2);
          True ->
            $w$j1_sIV
              (PS ww_sID ww1_sIE ww2_sIF ww3_sIG) __NULL $fMonoidByteString1 0 0
        };
      True -> Nothing
    }
```

# Benchmarks

```haskell
main = defaultMain [ bench "slower" (nf (runParser udp) "0000000000000000") ]
```

```
benchmarking slower
time                 38.58 ns   (38.54 ns .. 38.61 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 38.59 ns   (38.56 ns .. 38.63 ns)
std dev              118.0 ps   (86.80 ps .. 168.8 ps)
```

# More efficiency

```haskell
udp :: Parser UDP
udp = Parser (\bs0 ->
    if ByteString.length bs0 < 8
    then Nothing
    else do
        let (udpSourcePort     , bs1) = ByteString.splitAt 2 bs0
        let (udpDestinationPort, bs2) = ByteString.splitAt 2 bs1
        let (udpLength         , bs3) = ByteString.splitAt 2 bs2
        let (udpChecksum       , bs4) = ByteString.splitAt 2 bs3
        Just (UDP {..}, bs4) )
```

* One length check instead of four
* No more `Maybe` binds

# Still some waste

We still have some unnecessary branches:

```haskell
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt n ps@(PS x s l)
    | n <= 0    = (empty, ps)
    | n >= l    = (ps, empty)
    | otherwise = (PS x s n, PS x (s+n) (l-n))
```

... but we can eliminate them by using `unsafeTake` and `unsafeDrop` from
`Data.ByteString.Unsafe`:

```haskell
unsafeTake :: Int -> ByteString -> ByteString
unsafeTake n (PS x s l) = PS x s n

unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop n (PS x s l) = PS x (s+n) (l-n)
```

# More efficiency

```haskell
import qualified Data.ByteString.Unsafe as Unsafe

udp2 :: Parser UDP
udp2 = Parser (\bs0 ->
    if ByteString.length bs0 < 8
    then Nothing
    else do
        let (udpSourcePort     , bs1) = unsafeSplitAt 2 bs0
        let (udpDestinationPort, bs2) = unsafeSplitAt 2 bs1
        let (udpLength         , bs3) = unsafeSplitAt 2 bs2
        let (udpChecksum       , bs4) = unsafeSplitAt 2 bs3
        Just (UDP {..}, bs4) )

unsafeSplitAt :: Int -> ByteString -> (ByteString, ByteString)
unsafeSplitAt n bs =
    (Unsafe.unsafeTake n bs, Unsafe.unsafeDrop n bs)
```

However, this is very unsafe!

# Documented preconditions

Both `unsafeTake` and `unsafeDrop` document their preconditions:

```haskell
-- | A variety of 'take' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeTake :: Int -> ByteString -> ByteString

-- | A variety of 'drop' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeDrop  :: Int -> ByteString -> ByteString
```

Documentation is not machine-checked

# Liquid Haskell preconditions

We can teach Liquid Haskell to enforce those preconditions at compile-time:

```haskell
{-@ measure bslen :: ByteString -> Int @-}

{-@
assume ByteString.length :: bs : ByteString -> { n : Int | n == bslen bs }
@-}

{-@
Unsafe.unsafeTake
    :: n : { n : Int | 0 <= n }
    -> { i : ByteString | n <= bslen i }
    -> { o : ByteString | bslen o == n }
@-}

{-@
Unsafe.unsafeDrop
    :: n : { n : Int | 0 <= n }
    -> i : { i : ByteString | n <= bslen i }
    -> { o : ByteString | bslen o == bslen i - n }
@-}

{-@
unsafeSplitAt
    ::  n : { n : Int | 0 <= n }
    ->  i : { bs : ByteString | n <= bslen bs }
    ->  ( { bs : ByteString | bslen bs == n }
        , { bs : ByteString | bslen bs == bslen i - n }
        )
@-}
```

The precondition for `unsafeSplitAt` satisfies the precondition for `unsafeTake`
and `unsafeDrop`.

The postconditions for `unsafeDrop` and `unsafeTake` satisfy the postcondition
for `unsafeSplitAt`

# Refined datatypes

We can also add refinements to the UDP data type:

```haskell
{-@
data UDP = UDP
    { udpSourcePort      :: { bs : ByteString | bslen bs == 2 }
    , udpDestinationPort :: { bs : ByteString | bslen bs == 2 }
    , udpLength          :: { bs : ByteString | bslen bs == 2 }
    , udpChecksum        :: { bs : ByteString | bslen bs == 2 }
    }
@-}
data UDP = UDP
    { udpSourcePort      :: !ByteString  -- 2 bytes
    , udpDestinationPort :: !ByteString  -- 2 bytes
    , udpLength          :: !ByteString  -- 2 bytes
    , udpChecksum        :: !ByteString  -- 2 bytes
    }
```

Now we have machine-checked the field comments!

# SAFE

Now Liquid Haskell can verify that the following code is safe:

```haskell
udp :: ByteString -> Maybe (UDP, ByteString)
udp bs0 =
    if ByteString.length bs0 < 8
    then Nothing
    else do
        let (udpSourcePort     , bs1) = unsafeSplitAt 2 bs0
        let (udpDestinationPort, bs2) = unsafeSplitAt 2 bs1
        let (udpLength         , bs3) = unsafeSplitAt 2 bs2
        let (udpChecksum       , bs4) = unsafeSplitAt 2 bs3
        Just (UDP {..}, bs4)
```

```bash
$ liquid --totality example.hs
...
**** RESULT: SAFE **************************************************************
```

In this context, "safe" means:

* No out-of-bounds errors
* The UDP fields are all exactly 2 bytes long

# Full program

```haskell
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (take)
import Data.ByteString (ByteString)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as Unsafe

data Parser a = Parser { runParser :: ByteString -> Maybe (a, ByteString) }

{-@
take :: n : { n : Int | 0 <= n } -> Parser { bs : ByteString | bslen bs == n }
@-}
take :: Int -> Parser ByteString
take n = Parser (\bs ->
    if ByteString.length bs < n
    then Nothing
    else Just (ByteString.splitAt n bs) )

instance Functor Parser where
    fmap f p = Parser (\bs -> fmap adapt (runParser p bs))
      where
        adapt (x, bs) = (f x, bs)

instance Applicative Parser where
    pure x = Parser (\bs -> Just (x, bs))

    mf <*> mx = Parser (\bs0 -> do
        (f, bs1) <- runParser mf bs0
        let adapt (x, bs2) = (f x, bs2)
        fmap adapt (runParser mx bs1) )

instance Monad Parser where
    return x = Parser (\bs -> Just (x, bs))

    m >>= f = Parser (\bs0 -> do
        (x, bs1) <- runParser m bs0
        runParser (f x) bs1 )

{-@
data UDP = UDP
    { udpSourcePort      :: { bs : ByteString | bslen bs == 2 }
    , udpDestinationPort :: { bs : ByteString | bslen bs == 2 }
    , udpLength          :: { bs : ByteString | bslen bs == 2 }
    , udpChecksum        :: { bs : ByteString | bslen bs == 2 }
    }
@-}
data UDP = UDP
    { udpSourcePort      :: !ByteString
    , udpDestinationPort :: !ByteString
    , udpLength          :: !ByteString
    , udpChecksum        :: !ByteString
    }

udp :: Parser UDP
udp = do
    udpSourcePort      <- take 2
    udpDestinationPort <- take 2
    udpLength          <- take 2
    udpChecksum        <- take 2
    return (UDP {..})

udp2 :: Parser UDP
udp2 = Parser (\bs0 ->
    if ByteString.length bs0 < 8
    then Nothing
    else do
        let (udpSourcePort     , bs1) = unsafeSplitAt 2 bs0
        let (udpDestinationPort, bs2) = unsafeSplitAt 2 bs1
        let (udpLength         , bs3) = unsafeSplitAt 2 bs2
        let (udpChecksum       , bs4) = unsafeSplitAt 2 bs3
        Just (UDP {..}, bs4) )

{-@
unsafeSplitAt
    ::  n : { n : Int | 0 <= n }
    ->  i : { bs : ByteString | n <= bslen bs }
    ->  ( { bs : ByteString | bslen bs == n }
        , { bs : ByteString | bslen bs == bslen i - n }
        )
@-}
unsafeSplitAt :: Int -> ByteString -> (ByteString, ByteString)
unsafeSplitAt n bs =
    (Unsafe.unsafeTake n bs, Unsafe.unsafeDrop n bs)

{-@ measure bslen :: ByteString -> Int @-}

{-@
ByteString.length :: bs : ByteString -> { n : Int | n == bslen bs }
@-}

{-@
ByteString.splitAt
    :: n : Int
    -> i : ByteString
    -> ( { l : ByteString | (n <= 0 ==> bslen i == 0) &&
                            ((0 <= n && n <= bslen i) ==> bslen l == n) &&
                            (bslen i <= n ==> bslen l == bslen i)
         }
       , { r : ByteString | (n <= 0 ==> bslen r == bslen i) &&
                            ((0 <= n && n <= bslen i) ==> bslen r == bslen i - n) &&
                            (bslen i <= n ==> bslen r == 0)
         }
       )
@-}

{-@
Unsafe.unsafeTake
    :: n : { n : Int | 0 <= n }
    -> { i : ByteString | n <= bslen i }
    -> { o : ByteString | bslen o == n }
@-}

{-@
Unsafe.unsafeDrop
    :: n : { n : Int | 0 <= n }
    -> i : { i : ByteString | n <= bslen i }
    -> { o : ByteString | bslen o == bslen i - n }
@-}
```

# GHC Core

```haskell
$wa
$wa =
  \ ww_s1L4 ww1_s1L5 ww2_s1L6 ww3_s1L7 ->
    case tagToEnum# (<# ww3_s1L7 8) of _ {
      False ->
        let {
          a2_s1Ha
          a2_s1Ha = +# ww2_s1L6 2 } in
        let {
          a3_s1He
          a3_s1He = +# a2_s1Ha 2 } in
        let {
          a4_s1Hi
          a4_s1Hi = +# a3_s1He 2 } in
        Just
          (UDP
             (PS ww_s1L4 ww1_s1L5 ww2_s1L6 2)
             (PS ww_s1L4 ww1_s1L5 a2_s1Ha 2)
             (PS ww_s1L4 ww1_s1L5 a3_s1He 2)
             (PS ww_s1L4 ww1_s1L5 a4_s1Hi 2),
           PS
             ww_s1L4
             ww1_s1L5
             (+# a4_s1Hi 2)
             (-# (-# (-# (-# ww3_s1L7 2) 2) 2) 2));
      True -> Nothing
    }
```

# Benchmarks

```haskell
main :: IO ()
main = defaultMain
    [ bench "slower" (nf (runParser udp ) "0000000000000000")
    , bench "faster" (nf (runParser udp2) "0000000000000000")
    ]
```

```
benchmarking slower
time                 38.58 ns   (38.54 ns .. 38.61 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 38.59 ns   (38.56 ns .. 38.63 ns)
std dev              118.0 ps   (86.80 ps .. 168.8 ps)

benchmarking faster
time                 6.502 ns   (6.498 ns .. 6.506 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.503 ns   (6.500 ns .. 6.508 ns)
std dev              13.97 ps   (9.877 ps .. 19.63 ps)
```

The unchecked version is 6x faster (!), and provably safe

# Comparison to `cereal`

If you were wondering how `cereal` performs on the same task:

```
benchmarking faster
time                 6.502 ns   (6.498 ns .. 6.506 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.503 ns   (6.500 ns .. 6.508 ns)
std dev              13.97 ps   (9.877 ps .. 19.63 ps)

benchmarking cereal
time                 121.5 ns   (120.3 ns .. 122.8 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 123.1 ns   (121.1 ns .. 125.1 ns)
std dev              6.734 ns   (5.238 ns .. 8.969 ns)
variance introduced by outliers: 74% (severely inflated)
```

# `ByteString` annotations

Awake Networks believes in upstreaming and open sourcing as much as possible

We upstreamed a very large number of `ByteString` refinements

* [`Data.ByteString`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString.spec)
* [`Data.ByteString.Char8`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Char8.spec)
* [`Data.ByteString.Lazy`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Lazy.spec)
* [`Data.ByteString.Lazy.Char8`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Lazy/Char8.spec)
* [`Data.ByteString.Short`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Short.spec)
* [`Data.ByteString.Unsafe`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Unsafe.spec)

# Questions?

Next: Succinct vectors

# Succinct vectors

One of my pet projects is a high-performance succinct vector library:

* [Haskell-Succinct-Vector-Library](https://github.com/Gabriel439/Haskell-Succinct-Vector-Library)

The library implements [this paper](https://www.cs.cmu.edu/~dga/papers/zhou-sea2013.pdf)

This is a tricky algorithm prove correct:

* Full of corner cases
* Very non-trivial arithmetic to prove correctness at edge cases

Liquid Haskell clarified the algorithm in a way that the paper never could

# QuickCheck is not enough

QuickCheck is not good at sampling numbers far from 0 unless you tell it to:

```haskell
>>> quickCheck (\x -> x < 1000)
+++ OK, passed 100 tests.
>>> quickCheck (\(Large x) -> x < (1000 :: Int))
*** Failed! Falsifiable (after 19 tests and 6 shrinks):     
Large {getLarge = 1000}
```

Even then, `QuickCheck` still fails if the counter-example is rare:

```haskell
>>> quickCheck (\x -> x <= 1000 ==> x < 1000)
+++ OK, passed 100 tests.
```

# Real example

Unfortunately, the counterexamples I care about tend to be large and rare.

The algorithm requires working with blocks of dramatically different sizes:

* Level 0 block size is 2³² bits
* Level 1 block size is 2¹¹ bits

Counter examples are large (~ 2³²) and rare (~ 1 in 2²¹)

# Symbolic reasoning

Only symbolic reasoning can catch these mistakes:

```haskell
{-@
test
    :: x : { x : Int | x <= 1000 }
    ->     { x : Int | x <  1000 }
@-}
test :: Int -> Int
test x = x
```

```
 7 | test x = x
     ^^^^
 
   Inferred type
     VV : {VV : GHC.Types.Int | VV == x}
  
   not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV < 1000}
  
   In Context
     x := {v : GHC.Types.Int | v <= 1000}
```

Bleeding-edge Liquid Haskell has experimenta support for counter-examples

# Binary search in a vector

```haskell
{-@
search
    :: (Prim e, Ord a)
    => x  : a
    -> f  : (e -> a)
    -> v  : Data.Vector.Primitive.Vector e
    -> lo : { lo : Int | 0  <= lo && lo <  plen v }
    -> hi : { hi : Int | lo <  hi && hi <= plen v }
    ->      { r  : Int | lo <= r  && r  <  hi     }
    /  [hi - lo]
@-}
search
    :: (Ord a, Prim e)
    => a -> (e -> a) -> Data.Vector.Primitive.Vector e -> Int -> Int -> Int
search x f v lo hi = do
    if lo + 1 == hi
    then lo
    else do
        let mid = lo + (hi - lo) `div` 2
        let x' = f (Data.Vector.Primitive.unsafeIndex v mid)
        if x < x'
        then search x f v lo  mid
        else search x f v mid hi
```

# Example of `stack` integration

* Add `liquidhaskell` and dependencies to your `stack.yaml` file
* Run `stack exec liquid` on each source file you want to test

# Conclusions

* Liquid Haskell works well for high-performance `Vector`/`ByteString` code
* Liquid Haskell is not hard to integrate with `cabal` and `stack`
