% Scrap your Bounds Checks with Liquid Haskell 
% Gabriella Gonzalez
% October 12, 2017

# The golden rule of programming

**Prefer pushing fixes upstream over pushing problems downstream**

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

# Types + Predicates

Liquid Haskell is a backwards-compatible extension to Haskell's type system

You can use Liquid Haskell to add type-level predicates::

```haskell
{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x

{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then 0 - x else x
```

Liquid Haskell type signatures are comments alongside normal type signatures

# Test drive

If we type-check the following file with `liquid`:

```haskell
-- example.hs

import Prelude hiding (head, abs)

{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x

{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then 0 - x else x
```

... we get the following result:

```haskell
$ liquid --totality example.hs
...
**** RESULT: SAFE **************************************************************
```

# Overview

* **Liquid Haskell 101**
* High-performance parsing
* Ways to contribute
* Conclusion

# Preconditions and Postconditions

Liquid Haskell lets you annotate types with predicates

If you annotate function inputs you get preconditions:

```haskell
{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
```

If you annotate function outputs you get postconditions:

```haskell
{-@ abs :: Int -> { n : Int | 0 <= n } @-}
```

# Preconditions

Preconditions let you constrain function inputs:

```haskell
import Prelude hiding (head)

{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x

example :: Int
example = head []
```

Preconditions are "free" (i.e. you can always add more)

# Type error

Liquid Haskell will enforce those preconditions:

```
$ liquid precondition.hs
...
**** RESULT: UNSAFE ************************************************************


 /Users/gabriella/proj/slides/liquidhaskell/examples/head.hs:8:1-7: Error: Liquid Type Mismatch
 
 8 | example = head []
     ^^^^^^^
   Inferred type
     VV : {v : [Int] | len v == 0
                       && (null v <=> true)
                       && len v >= 0
                       && v == ?a
                       && len v >= 0}
  
   not a subtype of Required type
     VV : {VV : [Int] | 1 <= len VV}
  
   In Context
     ?a : {?a : [Int] | len ?a == 0
                        && (null ?a <=> true)
                        && len ?a >= 0}
```

# Non-invasive

Note that we made our code safer without any code changes

Other solutions to type-level safety usually require term-level changes like:

* Wrapping and unwrapping `newtype`s
* Using a different data structure (like using `NonEmpty` instead of `[]`)
* Using Template Haskell

Most of these are work-arounds for deficiencies in the type system

Why not **push the fix upstream** and fix the type system?

# Postconditions

Postconditions let you decorate function output with additional information:

```haskell
import Prelude hiding (head, abs)

{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then 0 - x else x

{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x

example :: Int -> Int
example x = if abs x < 0 then head [] else x
```

Postconditions are not "free" (i.e. you cannot add a false postcondition)

# Type check

Liquid Haskell uses postconditions to verify preconditions:

```
$ liquid --totality postcondition.hs
...
**** RESULT: SAFE **************************************************************
```

# Liquid Haskell is really smart

We can add a post-condition to `example`:

```haskell
import Prelude hiding (head, abs)

{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then 0 - x else x

{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x

{-@ example :: x : Int -> { y : Int | x == y } @-}
example :: Int -> Int
example x = if abs x < 0 then head [] else x
```

# Proving Postconditions

You cannot add a postcondition unless you can:

* Prove the postcondition from some precondition:

```haskell
example :: { x : Int | 0 <= x } -> { x : Int | 0 <= x }
example x = x
```

* Cheat and `assume` the postcondition

```haskell
{-@ assume (<) :: x : Int -> y : Int -> { z : Bool | z == x < y } @-}
{-@ assume (-) :: x : Int -> y : Int -> { z : Int  | z == x - y } @-}
```

* Prove the postcondition from another postcondition

```haskell
{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then 0 - x else x
```

# Walkthrough

```haskell
abs :: Int -> Int
abs x = if x < 0 then 0 - x else x

x :: Int

0 :: { n : Int | n == 0 }

x < 0 :: { b : Bool | b == x < 0 }

0 - x :: { n : Int | n == 0 - x }

if x < 0 then 0 - x else x
    :: { n : Int
       |    (      x < 0  => n == 0 - x )
         && ( not (x < 0) => n == x     )
       }
```

The inferred type is stronger than the postcondition we originally requested:

```haskell
if x < 0 then 0 - x else x
    :: { n : Int | 0 <= n }
```

# SMT solver

How does Liquid Haskell know that the inferred type:

```haskell
if x < 0 then 0 - x else x
    :: { n : Int
       |    (      x < 0  => n == 0 - x )
         && ( not (x < 0) => n == x     )
       }
```

... is a subtype of the user-supplied type:

```haskell
if x < 0 then 0 - x else x
    :: { n : Int | 0 <= n }
```

Liquid Haskell uses an SMT solver (such as Z3) to check these subtype relations

Think of Liquid Haskell as a more user-friendly subset of dependent types

# Informative types

Can you guess what this function does just from the type?

```haskell
mysteryFunction
    :: n : { n : Int | 0 <= n }
    -> xs : { xs : [a] | n <= len xs }
    -> ({ ls : [a] | len ls == n }, { rs : [a] | len rs = len xs - n })
```

# `splitAt`

```haskell
{-@
splitAt
    :: n : { n : Int | 0 <= n }
    -> xs : { xs : [a] | n <= len xs }
    -> ({ ls : [a] | len ls == n }, { rs : [a] | len rs = len xs - n })
@-}
splitAt :: Int -> [a] -> ([a], [a])
splitAt 0  xs    = (  [], xs)
splitAt n (x:xs) = (x:ls, rs)
  where
    ~(ls, rs) = splitAt (n - 1) xs
```

Liquid Haskell verifies that the non-exhaustive pattern match is safe!

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

{-@ assume ByteString.pack :: cs : [Char] -> { bs : ByteString | bslen bs == len cs } @-}

{-@ ByteString.head :: { bs : ByteString | 1 <= bslen bs } -> Char @-}

staticCheck :: Char
staticCheck = head "Hello, world!"
```

We **pushed the fix upstream**!

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

# Learning Liquid Haskell

* 20% is actually learning how to prove things in Liquid Haskell
    * This is pretty easy because Liquid Haskell does most of the work for you
* 40% is reporting bugs that you encounter 
    * The maintainers are very responsive and actively fix reported bugs
* 40% is building a mental model of what Liquid Haskell can and cannot do
    * This is extra hard because Liquid Haskell continues to grow more powerful

# Scrap your bounds checks

```haskell
import Data.Vector (Vector)

import qualified Data.Vector

{-@
search
    :: Ord a
    => element : a
    -> vector : Vector a
    -> minIndex : { minIndex : Int | 0  <= minIndex && minIndex <  vlen vector }
    -> maxIndex : { maxIndex : Int | minIndex <  maxIndex && maxIndex <= vlen vector }
    ->            {    index : Int | minIndex <= index && index < maxIndex }
    /  [maxIndex - minIndex]
@-}
search :: Ord a => a -> Vector a -> Int -> Int -> Int
search element vector minIndex maxIndex
    | minIndex + 1 == maxIndex = minIndex
    | element < testElement    = search element vector minIndex testIndex
    | otherwise                = search element vector testIndex maxIndex
  where
    testIndex = (minIndex + maxIndex) `div` 2

    testElement = Data.Vector.unsafeIndex vector testIndex
```

What if I call `search` this on an empty list?

# Questions?

* Liquid Haskell 101
* **High-performance parsing**
* Ways to contribute
* Conclusion

# Packet header parsing

At Awake Security we use Haskell for high-performance protocol parsing

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

instance Functor     Parser where ...
instance Applicative Parser where ...
instance Monad       Parser where
    return x = Parser (\bs -> Just (x, bs))

    m >>= f = Parser (\bs0 -> do
        (x, bs1) <- runParser m bs0
        runParser (f x) bs1 )

```

# UDP packet header

```haskell
{-# LANGUAGE RecordWildCards #-}

-- We could totally use a more space-efficient type than this, but humor me
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

We're wastefully checking the length of the `ByteString` eight times!

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

Four length checks instead of eight

# GHC Core

```haskell
$wa1
$wa1 =
  \ ww_s1EN ww1_s1EO ww2_s1EP ww3_s1EQ ->
    case tagToEnum# (<# ww3_s1EQ 8) of _ {
      False ->
        let {
          ds_s1wb
          ds_s1wb =
            case tagToEnum# (>=# 2 ww3_s1EQ) of _ {
              False ->
                (PS ww_s1EN ww1_s1EO ww2_s1EP 2,
                 PS ww_s1EN ww1_s1EO (+# ww2_s1EP 2) (-# ww3_s1EQ 2));
              True -> (PS ww_s1EN ww1_s1EO ww2_s1EP ww3_s1EQ, empty)
            } } in
        let {
          ds1_s1wa
          ds1_s1wa =
            case ds_s1wb of _ { (udpSourcePort1_a1is, bs1_X1jt) ->
            case bs1_X1jt
            of wild2_a1u6 { PS dt_a1u8 dt1_a1u9 dt2_a1ua dt3_a1ub ->
            case tagToEnum# (>=# 2 dt3_a1ub) of _ {
              False ->
                (PS dt_a1u8 dt1_a1u9 dt2_a1ua 2,
                 PS dt_a1u8 dt1_a1u9 (+# dt2_a1ua 2) (-# dt3_a1ub 2));
              True -> (wild2_a1u6, empty)
            }
            }
            } } in
        let {
          ds2_s1w9
          ds2_s1w9 =
            case ds1_s1wa of _ { (udpDestinationPort1_a1iC, bs2_X1jL) ->
            case bs2_X1jL
            of wild2_a1u6 { PS dt_a1u8 dt1_a1u9 dt2_a1ua dt3_a1ub ->
            case tagToEnum# (>=# 2 dt3_a1ub) of _ {
              False ->
                (PS dt_a1u8 dt1_a1u9 dt2_a1ua 2,
                 PS dt_a1u8 dt1_a1u9 (+# dt2_a1ua 2) (-# dt3_a1ub 2));
              True -> (wild2_a1u6, empty)
            }
            }
            } } in
        let {
          ds3_s1w8
          ds3_s1w8 =
            case ds2_s1w9 of _ { (udpLength1_a1iL, bs3_X1k2) ->
            case bs3_X1k2
            of wild2_a1u6 { PS dt_a1u8 dt1_a1u9 dt2_a1ua dt3_a1ub ->
            case tagToEnum# (>=# 2 dt3_a1ub) of _ {
              False ->
                (PS dt_a1u8 dt1_a1u9 dt2_a1ua 2,
                 PS dt_a1u8 dt1_a1u9 (+# dt2_a1ua 2) (-# dt3_a1ub 2));
              True -> (wild2_a1u6, empty)
            }
            }
            } } in
        Just
          (case ds_s1wb of _ { (udpSourcePort1_X1jt, bs1_X1iV) ->
           case udpSourcePort1_X1jt
           of dt_XQ9 { PS ipv_s1ym ipv1_s1yn ipv2_s1yo ipv3_s1yp ->
           case ds1_s1wa of _ { (udpDestinationPort1_X1jL, bs2_X1j6) ->
           case udpDestinationPort1_X1jL
           of dt1_XQb { PS ipv4_s1yt ipv5_s1yu ipv6_s1yv ipv7_s1yw ->
           case ds2_s1w9 of _ { (udpLength1_X1k2, bs3_X1jg) ->
           case udpLength1_X1k2
           of dt2_XQd { PS ipv8_s1yA ipv9_s1yB ipv10_s1yC ipv11_s1yD ->
           case ds3_s1w8 of _ { (udpChecksum1_X1kj, bs4_X1jq) ->
           case udpChecksum1_X1kj
           of dt3_XQf { PS ipv12_s1yH ipv13_s1yI ipv14_s1yJ ipv15_s1yK ->
           UDP dt_XQ9 dt1_XQb dt2_XQd dt3_XQf
           }
           }
           }
           }
           }
           }
           }
           },
           case ds3_s1w8 of _ { (udpChecksum1_a1iU, bs4_X1kj) -> bs4_X1kj });
      True -> Nothing
    }
```

# Still some waste

Our four remaining length checks come from the use of `splitAt`:

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
{-@
assume ByteString.length :: bs : ByteString -> { n : Int | n == bslen bs }
@-}

{-@
assume Unsafe.unsafeTake
    :: n : { n : Int | 0 <= n }
    -> { i : ByteString | n <= bslen i }
    -> { o : ByteString | bslen o == n }
@-}

{-@
assume Unsafe.unsafeDrop
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

Awake Security believes in upstreaming and open sourcing as much as possible

We upstreamed a very large number of `ByteString` refinements

* [`Data.ByteString`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString.spec)
* [`Data.ByteString.Char8`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Char8.spec)
* [`Data.ByteString.Lazy`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Lazy.spec)
* [`Data.ByteString.Lazy.Char8`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Lazy/Char8.spec)
* [`Data.ByteString.Short`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Short.spec)
* [`Data.ByteString.Unsafe`](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/ByteString/Unsafe.spec)

# Questions?

* Liquid Haskell 101
* High-performance parsing
* **Ways to contribute**
* Conclusion

# Liquid Haskell Prelude

Liquid Haskell has a "Prelude" of refined types for common libraries

* [Liquid Haskell Prelude](https://github.com/ucsd-progsys/liquidhaskell/tree/develop/include)

The easiest way to contribute to Liquid Haskell is to add new refinements!

Low-hanging fruit:

* [Data/Vector.spec](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Data/Vector.spec)

Let's create a pull request to show how easy it is!

# Report bugs

You **will** run into bugs if you use Liquid Haskell for any non-trivial project

Reporting bugs is a valuable contribution!

# Add `liquidhaskell` to Stackage

The maintainers can use your help getting `liquidhaskell` on Stackage

* Upgrading `liquidhaskell` to use newer versions of libraries (like `aeson`)
* Getting dependencies on Hackage (like `liquid-fixpoint`)

# Improve distribution

Create a `liquidhaskell` package for your favorite package manager

Bonus: the package can bundle `liquidhaskell` with an SMT solver (like `z3`)

# Triage or fix issues

At the time of this writing, Liquid Haskell has 238 open issues

* Turn bug reports into failing tests (way more useful than you think!)
* Identify stale issues that are already fixed
* Fix issues if you can!

# Improve performance

Liquid Haskell can take a while to type-check certain programs

Improving performance allows Liquid Haskell to scale to larger codebases

# GHC integration

One long-term goal of Liquid Haskell is direct integration with `ghc`

This is a challenging, but highly worthwhile project

* Drafting a proposal
* Reconciling Liquid Haskell's types with upcoming dependent types
* Optionally packaging an SMT solver with `ghc`

Why not **push the fix upstream** and fix GHC directly?

# Questions?

* Liquid Haskell 101
* High-performance parsing
* Ways to contribute
* **Conclusion**

# Conclusions

* Liquid Haskell works well for high-performance `Vector`/`ByteString` code
* You can integrate Liquid Haskell with `cabal` and `stack`
* The Liquid Haskell project needs your help!

These slides are hosted online at:

* [https://github.com/Gabriella439/slides/blob/master/liquidhaskell/slides.md](https://github.com/Gabriella439/slides/blob/master/liquidhaskell/slides.md)

You can follow my work on:

* Twitter - [\@GabriellaG439](twitter.com/GabriellaG439)
* GitHub - [Gabriella439](https://github.com/Gabriella439)

# Getting started - Stack

Create this `stack.yaml` file:

```yaml
resolver: lts-9.5
packages: []
extra-deps:
- aeson-0.11.3.0
- dotgen-0.4.2
- fgl-visualize-0.1.0.1
- intern-0.9.1.4
- liquid-fixpoint-0.6.0.1
- liquid-haskell-0.8.0.1
- located-base-0.1.1.1
```

... then in the same directory run:

```bash
$ stack install liquidhaskell
```

Now the `liquid` executable will installed at `~/.local/bin/liquid`

You still need to separately install `z3`

# Getting started - Nix

```
$ nix-shell
```

... using the following `shell.nix` file:

```nix
let
  inherit (import <nixpkgs> { }) fetchFromGitHub;

  nixpkgs = fetchFromGitHub {
    owner = "NixOS";

    repo = "nixpkgs";

    rev = "1715436b75696d9885b345dd8159e12244d0f7f5";
    sha256 = "18qp76cppm1yxmzdaak9kcllbypvv22c9g7iaycq2wz0qkka6rx5";
  };

  pkgs = import nixpkgs { };

  liquid =
    pkgs.runCommand "liquidhaskell" { buildInputs = [ pkgs.makeWrapper ]; } ''
      mkdir -p $out/bin
      ln -s ${pkgs.haskellPackages.liquidhaskell}/bin/liquid $out/bin
      wrapProgram $out/bin/liquid --prefix PATH : ${pkgs.z3}/bin
    '';

  ghc = pkgs.haskellPackages.ghcWithPackages (packages: with packages; [
    vector
  ]);

in
  pkgs.stdenv.mkDerivation {
    name = "my-haskell-env-0";
    buildInputs = [ ghc liquid ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  }
```

# `liquidhaskell-cabal`

You can integrate Liquid Haskell into any project built using `cabal` or stack`

Awake Security maintains a working fork of the `liquidhaskell-cabal` project:

* [https://github.com/awakesecurity/liquidhaskell-cabal](https://github.com/awakesecurity/liquidhaskell-cabal)

The project `README` has instructions on how to use this in your own projects
