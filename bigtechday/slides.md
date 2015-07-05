% How to prove large software projects correct
% Gabriel Gonzalez
% Big Tech Day - June 12, 2015

# Goal

We would like to prove large software projects correct:

... without a proof assistant

... without a type-checker

... without a computer!

# Approach

We will approach complex proofs using the Unix philosophy:

* Prove one thing and prove it well

* Compose smaller proofs to build larger proofs

* Everything is a ~~String~~ Monoid

# Overview

* **Brief introduction to Haskell**

* Prove one thing and prove it well

* Compose smaller proofs to build larger proofs

* Everything is a ~~String~~ Monoid

* Conclusion

# Haskell

I will be using the Haskell programming language for all examples, because:

* Haskell is the most widely used purely functional language
* I want the examples to fit on the slides

I will teach the language as I go along

# Equational reasoning

Traditional formal methods:

* Define a formal language for expressing properties about your program
* Create a specification for your program in this formal language
* Verify that your program meets this specification

Equational reasoning:

* The programming language **is** your formal language
* You define equations directly within the programming language
* You specify desired behavior in terms of equations

# Equational reasoning example

We specify the behavior of `(&&)` in terms of two equations:

```haskell
False && x = False  -- Equation #1
True  && x = x      -- Equation #2
```

Then we can use the above two equations to prove properties about `(&&)`:

```haskell
y && True = y

-- Case #1: y = True
True && True = True    -- (2)

-- Case #2: y = False
False && True = False  -- (1)

-- QED
```

These equations are "executable"!

```haskell
>>> True && False
False
```

# Purely functional programming

"Purely functional" languages support equational reasoning:

* Haskell
* Purescript
* Elm
* Idris

These languages lower the barrier to formal reasoning.  You learn two languages
for the price of one: a programming language and a formal language.

# Equations

Haskell programs are just equations

For example, you specify what to run by equating `main` with a subroutine

```haskell
-- example.hs

main :: IO ()
main = putStrLn "Hello, world!"
```

```bash
$ ghc -O2 example.hs
$ ./example
Hello, world!
```

```haskell
putStrLn :: String -> IO ()
```

# Functional

Haskell functions are defined using equations:

```haskell
exclaim x = x ++ "!"

main = putStrLn (exclaim "Hello, world")
```

```bash
$ ./example
Hello, world!
```

# Equality

```haskell
exclaim x = x ++ "!"
```

Equality means that everywhere we see `exclaim x` we can replace it with
`x ++ "!"`:

```haskell
putStrLn (exclaim "Hello, world")

-- exclaim x = x ++ "!"
= putStrLn ("Hello, world" ++ "!")
```

Vice versa, anywhere we see `x ++ "!"`, we can replace it with `exclaim x`:

```haskell
putStrLn ("Goodbye, world" ++ "!")

-- "Goodbye, world" ++ "!" = exclaim "Goodbye, world"
= print (exclaim "Goodbye, world")
```

Bidirectional substitution is safe because evaluation is benign in Haskell;
evaluation does not trigger side effects.

# Functions versus subroutines

* Functions have arguments, but no side effects

```haskell
even :: Int -> Bool          -- A function from `Int` to `Bool`
```

* Subroutines have side effects, but no arguments

```haskell
getLine :: IO String         -- A subroutine that returns a `String`
```

What about `putStrLn`?

```haskell
putStrLn :: String -> IO ()  -- A function from `String` to a subroutine
```

`putStrLn` is a **pure** function whose result is a subroutine

Evaluating `putStrLn` does not trigger the subroutine's side effects!

The only way to run a side effect is to equate the side effect with `main`.

# Side effects

```haskell
x = putStrLn "Goodbye, world!"

main = putStrLn "Hello, world!"
```

```bash
$ ./example
Hello, world!
```

Even if we force evaluation of `x`, nothing changes:

```haskell
main = x `seq` putStrLn "Hello, world!"
```

So how do we execute more than one side effect?

# Algebraic

Haskell design patterns are "algebraic"

Informally, "algebraic" means that when you combine things you end up back
where you started.

```haskell
(>>) :: IO () -> IO () -> IO ()

-- (x >> y) >> z = x >> (y >> z)
```

```haskell
main = putStrLn "Hello, world!" >> putStrLn "Goodbye, world!"
```

```bash
$ ./example
Hello, world!
Goodbye, world!
```

We **don't** instrument `main` to accept lists of subroutines.  Instead, we
combine **smaller** subroutines into a **larger** subroutine.

# `do` notation

```haskell
main = do
    putStrLn "Hello, world!"
    putStrLn "Goodbye, world!"
```

... desugars to:

```haskell
main = putStrLn "Hello, world!" >> putStrLn "Goodbye, world!"
```

# `do` notation

```haskell
main = do
    str <- getLine
    putStrLn str
```

... desugars to:

```haskell
main = getLine >>= (\str -> putStrLn str)

-- same as:
main = getLine >>= putStrLn
```

```haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```

```haskell
getLine  :: IO String
putStrLn :: String -> IO ()

getLine >>= putStrLn :: IO ()
```

# The empty program

What if we have nothing to run?

```haskell
return :: a -> IO a
```

```haskell
main = return ()
```

# Questions?

* Brief introduction to Haskell

* **Prove one thing and prove it well**

* Compose smaller proofs to build larger proofs

* Everything is a ~~String~~ Monoid

* Conclusion

# A simple program

Let's begin with a simple Haskell program:

```haskell
main = do
    putStrLn "!"
    putStrLn "!"
    putStrLn "!"
```

This obviously prints `"!"` three times:

```haskell
$ ./example
!
!
!
```

# Don't repeat yourself

We can use the `replicateM_` utility from Haskell's standard library to run a
command multiple times:

```haskell
-- Simplified a little from the original definition
replicateM_ :: Int -> IO () -> IO ()
replicateM_ 0 io = return ()
replicateM_ n io = io >> replicateM_ (n - 1) io
```

Now we can write:

```haskell
import Control.Monad (replicateM_)

main = replicateM_ 3 (putStrLn "!")
```

... and the program behaves the same:

```bash
$ ./example
!
!
!
```

... but is this really equal to the previous program?

# The specification

We wish to prove that:

```haskell
replicateM_ 3 (putStrLn "!") = putStrLn "!" >> putStrLn "!" >> putStrLn "!"
```

We could prove this using the two equations that define `replicateM_`:

```haskell
replicateM_ 0 io = return ()                     -- Equation #1
replicateM_ n io = io >> replicateM_ (n - 1) io  -- Equation #2
```

```haskell
replicateM_ 3 (putStrLn "!")
= putStrLn "!" >> replicateM_ 2 (putStrLn "!")                                 -- (2)
= putStrLn "!" >> putStrLn "!" >> replicateM_ 1 (putStrLn "!")                 -- (2)
= putStrLn "!" >> putStrLn "!" >> putStrLn "!" >> replicateM_ 0 (putStrLn "!") -- (2)
= putStrLn "!" >> putStrLn "!" >> putStrLn "!" >> return ()                    -- (1)
-- (io :: IO ()) >> return () = io
= putStrLn "!" >> putStrLn "!" >> putStrLn "!"
```

However, this won't scale to larger programs

# Proof agility

For example, try proving that:

```haskell
replicateM_ 5 (replicateM_ 4 io) = replicateM_ 10 io >> replicateM_ 10 io
```

Intuitively, you "know" it's true, but why?

# Higher-level equations

We can make our "intuition" precise by using four equations:

```haskell
replicateM_  0      io = return ()

replicateM_ (x + y) io = replicateM_ x io >> replicateM_ y io

replicateM_  1      io = io

replicateM_ (x * y) io = replicateM_ x (replicateM_ y io)
```

We can write the latter two equations in a more point-free form:

```haskell
replicateM_  1      = id
replicateM_ (x * y) = replicateM_ x . replicateM_ y

id :: a -> a
id x = x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
```

# Reasoning algebraically

```haskell
replicateM_ 5 (replicateM_ 4 io)

= replicateM_ (5 * 4) io

= replicateM_ 20 io

= replicateM_ (10 + 10) io

= replicateM_ 10 io >> replicateM_ 10 io
```

We reduced a complex manipulation to a simpler manipulation

# Reasoning algebraically

```haskell
replicateM_ 3 io

-- 3 = 1 + 1 + 1
= replicateM_ (1 + 1 + 1) io

-- replicateM_ (x + y) io = replicateM_ x io >> replicateM_ y io
= replicateM_ 1 io >> replicateM_ 1 io >> replicateM_ 1 io

-- replicateM_ 1 io = io
= io >> io >> io
```

# Questions?

* Brief introduction to Haskell

* Prove one thing and prove it well

* **Compose smaller proofs to build larger proofs**

* Everything is a ~~String~~ Monoid

* Conclusion

# The pattern

Assume we have a "simple" associative operator named `(+₁)` with identity `0₁`:

```haskell
(x +₁ y) +₁ z = x +₁ (y +₁ z)

x  +₁ 0₁ = x

0₁ +₁ x  = x
```

... and a "complex" associative operator named `(+₂)` with identity
`0₂`:

```haskell
(x +₂ y) +₂ z = x +₂ (y +₂ z)

x  +₂ 0₂ = x

0₂ +₂ x  = x
```

... and a function that bridges between the two operators and their identities::

```haskell
map (x +₁ y) = (map x) +₂ (map y)

map 0₁       = 0₂
```

We can use `f` to bridge between complex code using `(+₂)`/`0₂` and simpler code
using `(+₁)`/`0₁`

# `map`

```haskell
map :: (a -> b) -> [a] -> [b]
map f  []    = []
map f (x:xs) = (f x):(map f xs)
```

```haskell
>>> map (+ 1) [1,2,3]
[2,3,4]
```

# `map`

```haskell
map = map

(+₁) = (.)
 0₁  = id

(+₂) = (.)
 0₂  = id
```

```haskell
map (f . g) = map f . map g
map  id     = id
```

# `map`

```haskell
map = map f

(+₁) = (++)
 0₁  =  []

(+₂) = (++)
 0₂  =  []
```

```haskell
map f (xs ++ ys) = (map f xs) ++ (map f ys)
map f  []        = []
```

# `replicateM_`

```haskell
map x = replicatem_ x io

(+₁) = (+)
 0₁  =  0

(+₂) = (>>)
 0₂  = return ()
```

```haskell
replicatem_ (x + y) io = replicatem_ x io >> replicatem_ y io
replicateM_  0      io = return ()
```

# `replicateM_`

```haskell
map = replicateM_

(+₁) = (*)
 0₁  =  1

(+₂) = (.)
 0₂  = id
```

```haskell
replicateM_ (x * y) = replicateM_ x . replicateM_ y
replicateM_  1      = id
```

# This pattern is everywhere

```haskell
length :: [a] -> Int
length (xs ++ ys) = (length xs) + (length ys)
length  []        = 0

-- Assuming: x, y >= 0
replicate :: Int -> a -> [a]
replicate (x + y) a = replicate x a ++ replicate y a
replicate  0      a = []

not :: Bool -> Bool
not (x && y) = (not x) || (not y)
not  True    = False

exp :: Double -> Double
exp (x + y) = (exp x) * (exp y)
exp  0      = 1

null :: [a] -> Bool
null (xs ++ ys) = (null xs) && (null ys)
null  []        = True

putStr :: String -> IO ()
putStr (str1 ++ str2) = putStr str1 >> putStr str2
putStr  ""            = return ()
```

# Chaining proofs

```haskell
-- Assuming: x, y > 0

not (null (replicate x a)) || not (null (replicate y a))

= not (null (replicate x a) && null (replicate y a))

= not (null (replicate x a ++ replicate y a))

= not (null (replicate (x + y) a))
```

# Questions?

* Brief introduction to Haskell

* Prove one thing and prove it well

* Compose smaller proofs to build larger proofs

* **Everything is a ~~String~~ Monoid**

* Conclusion

# Monoid

```haskell
class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- (x <> y) <> z = x <> (y <> z)
-- 
-- x <> mempty = x
-- 
-- mempty <> x = x
```

```haskell
instance Monoid [a] where
    mempty  = []
    mappend = (++)
```

```haskell
>>> [1,2] <> [3,4]
[1,2,3,4]
>>> mempty :: [Int]
[]
```

# The unit `Monoid`

```haskell
instance Monoid () where
    mempty = ()

    mappend () () = ()
```

```
>>> () <> ()
()
>>> mempty :: ()
()
```

# The pair `Monoid`

```haskell
instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)

    mappend (xL, yL) (xR, yR) = (mappend xL xR, mappend yL, yR)
```

```haskell
>>> ([1,2],[5,6]) <> ([3,4],[7,8])
([1,2,3,4],[5,6,7,8])
>>> mempty :: ([Int], [Int])
([],[])
```

# The function `Monoid`

```haskell
instance Monoid b => Monoid (a -> b) where
    mempty = \_ -> mempty

    mappend f g = \x -> mappend (f x) (g x)
```

```haskell
>>> (id <> reverse) [1,2,3,4]
[1,2,3,4,4,3,2,1]
>>> mempty (True, "story") :: [Int]
[]
```

# The `IO` `Monoid`

```haskell
instance Monoid b => Monoid (IO b) where
    mempty = return mempty

    mappend io1 io2 = do
        r1 <- io1
        r2 <- io2
        return (mappend r1 r2)
```

```haskell
readLn :: Read a => IO a
```

```haskell
>>> readLn :: IO [Int]
[1,2]<Enter>
[1,2]
>>> readLn <> readLn :: IO [Int]
[1,2]<Enter>
[3,4]<Enter>
[1,2,3,4]
>>> mempty :: IO [Int]
[]
```

# Combining `Monoid`s

The type of `putStrLn` is a `Monoid`

```haskell
putStrLn :: String -> IO ()
```

Here is how the compiler deduces that this is a `Monoid`:

* `()` is a `Monoid`

```haskell
instance Monoid () where ...
```

* If `()` is a `Monoid`, then `IO ()` is a `Monoid`

```haskell
instance Monoid b => Monoid (IO b) where ...
```

* If `IO ()` is a `Monoid`, then `String -> IO ()` is a `Monoid`

```haskell
instance Monoid b => Monoid (a -> b)
```

Let's try this out:

```haskell
>>> (putStrLn <> putStrLn) "Print me twice"
Print me twice
Print me twice
```

# Chaining monoids

These are all `Monoid`s:

* Nested subroutines:

```haskell
IO (IO (IO (IO (IO (IO ())))))
```

* Functions of multiple arguments:

```haskell
Name ->  Age ->  Address -> [People]

Name -> (Age -> (Address -> [People]))
```

* Deeply nested tuples:

```haskell
(([Int], [Int]), ([Int], [Int]))
```

* All of the above:

```haskell
IO (Config -> [IO ([Textures], [Models])])
```

# Induction on `Monoid`s

There's no limit to how many layers we can chain.

For example, this is a valid monoid:

```haskell
-- +-- Generate files
-- |
-- |   +-- Package them
-- |   |
-- |   |   +-- Upload 
-- |   |   |
-- |   |   |   +-- Deploy
-- |   |   |   |
-- |   |   |   |   +-- Log
-- |   |   |   |   |
-- v   v   v   v   v
   IO (IO (IO (IO (IO ()))))

-- join :: IO (IO a) -> IO a

deploy :: IO (IO (IO (IO (IO ())))) -> IO ()
deploy = join . join . join . join
```

# Staged deploy

```haskell
import Control.Monad (join)
import Data.Monoid (Monoid(..), (<>))

instance Monoid b => Monoid (IO b) where
    mempty          = return mempty
    mappend io1 io2 = do
        r1 <- io1
        r2 <- io2
        return (mappend r1 r2)

deploy :: IO (IO (IO (IO (IO ())))) -> IO ()
deploy = join . join . join . join

job :: Int -> IO (IO (IO (IO (IO ()))))
job n = do
    putStrLn ("Generating job #" <> show n)
    return (do
        putStrLn ("Packaging job #" <> show n)
        return (do
            putStrLn ("Uploading job #" <> show n)
            return (do
                putStrLn ("Deploying job #" <> show n)
                return (do
                    putStrLn ("Logging job #" <> show n)
                    return () ) ) ) )
```

# Interleaved deploy

Combining commands interleaves their phases:

```haskell
>>> deploy (job 1 <> job 2 <> job 3)
Generating job #1
Generating job #2
Generating job #3
Packaging job #1
Packaging job #2
Packaging job #3
Uploading job #1
Uploading job #2
Uploading job #3
Deploying job #1
Deploying job #2
Deploying job #3
Logging job #1
Logging job #2
Logging job #3
```

The empty job does nothing:

```haskell
>>> deploy mempty
>>> -- Nothing happens
```

# The power of induction

We wrote a staged deploy system:

... in two lines of code (the type signature was optional):

```haskell
import Control.Monad (join)

deploy = join . join . join . join
```

... that is well-behaved:

```haskell
(job1 <> job2) <> job3 = job1 <> (job2 <> job3)

job <> mempty = job

mempty <> job = job
```

... and we can easily prove correctness by just doing induction on the type:

```haskell
IO (IO (IO (IO (IO ()))))
```

# Terminal plugins 

The type of a plugin will be:

```haskell
-- +-- Acquire resources
-- |
-- |                  +-- Free resources
-- |   Char handler   |
-- v   vvvvvvvvvvvvv  v
   IO (Char -> IO (), IO ())

sinkTo :: IO (Char -> IO (), IO ()) -> IO ()
sinkTo open = bracket open snd (\(handle, _) -> do
    hSetEcho False
    let loop = do
            eof <- isEOF
            unless eof (do
                char <- getChar
                handle char
                loop )
    loop )
```

# Example plugins

```haskell
console :: IO (Char -> IO (), IO ())
console = return (putChar, mempty)

file :: FilePath -> IO (Char -> IO (), IO ())
file path = do
    handle <- openFile path WriteMode
    return (hPutChar handle, hClose handle)

counter :: IO (Char -> IO (), IO ())
counter = do
    counter <- newIORef (0 :: Int)
    let handle char = do
            n <- readIORef counter
            print n
            writeIORef counter (n + 1)
    return (handle, mempty)

main = sinkTo (console <> file "test.txt" <> file "test2.txt")
--   = sinkTo (counter <> file "test.txt" <> file "test2.txt")
```

# Questions?

* Brief introduction to Haskell

* Prove one thing and prove it well

* Compose smaller proofs to build larger proofs

* Everything is a ~~String~~ Monoid

* **Conclusion**

# Prove one thing and prove it well

We build complex systems by connecting together small, provably correct pieces:

* `IO`
* `(a -> b)`
* `(a, b)`
* `[a]`

These are analogous to Unix utilities like:

* `wc`
* `head`
* `grep`
* `cat`

# Compose smaller proofs to build larger proofs

We build larger components by connecting smaller components:

* `IO (IO (IO (IO (IO ()))))`
* `IO (Char -> IO (), IO ())`

Composition of components preserves correctness

# "Everything is a Monoid"

We use `Monoid` as a reusable and general interface for serendipitous
composition of proofs

Each proof takes a `Monoid` instance as its input and produces a `Monoid`
instance as its output:

```haskell
instance Monoid b => Monoid (IO b) where
```

# Conclusion

We can use the Unix philosophy to scale proofs to complex software:

* Prove one thing and prove it well

* Compose smaller components to build larger components

* Everything is a ~~String~~ Monoid

My blog: [haskellforall.com](http://haskellforall.com)

Twitter: [\@GabrielG439](http://twitter.com/GabrielG439)
