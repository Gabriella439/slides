% Namespaced De Bruijn indices
% Gabriella Gonzalez
% April 16, 2022

# Context

<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@4/distr/fira_code.css">
<style>
code {
  font-family: "Fira Code";
}
</style>

This talk is a longer presentation based on a blog post I published:

* [`haskellforall.com` - Namespaced De Bruijn indices](https://www.haskellforall.com/2021/08/namespaced-de-bruijn-indices.html)

In this talk I'll spend more time focusing on:

* the motivation, history and logical progression leading up to this idea
* the user experience (as opposed to the implementation)
* how [Dhall][dhall] uses this trick to great effect

As the blog post notes,
[CINNI](https://www.sciencedirect.com/science/article/pii/S1571066105801252)
predates this work and presents exactly the same idea

Also, Mike Shulman describes a subset of this trick in [You Could Have Invented De Bruijn indices](https://golem.ph.utexas.edu/category/2021/08/you_could_have_invented_de_bru.html)

# Overview

* Strong normalization
* Π types
* Name preservation
* Nameless and named representations

# Background

I first stumbled upon this idea while working on
[Morte](https://github.com/Gabriel439/Haskell-Morte-Library/issues/1), which is:

* essentially the calculus of constructions + an import system
* a proof of concept of my [internet of code](https://www.haskellforall.com/2015/05/the-internet-of-code.html) idea
* the predecessor of [Dhall][dhall]

Both Morte and Dhall support
[strong normalization](https://en.wikipedia.org/wiki/Normal_form_(abstract_rewriting))

# Motivation - Strong normalization

Both Morte and Dhall are
[total functional programming languages](https://en.wikipedia.org/wiki/Total_functional_programming)

This means all expressions can be β-reduced to a normal form.  For example:

```haskell
⊢ :let List/generate = https://prelude.dhall-lang.org/List/generate.dhall

List/generate : ∀(n : Natural) → ∀(a : Type) → ∀(f : Natural → a) → List a

⊢ List/generate 10 Bool Natural/even

[ True, False, True, False, True, False, True, False, True, False ]

⊢ List/generate 10

λ(a : Type) →
λ(f : Natural → a) →
  [ f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8, f 9 ]
```

Strong normalization lets you interpret incomplete code

# Strong normalization ⇒ code comprehension

Strong normalization eliminates indirection

For example, consider the `Natural/greaterThan` function from Dhall's Prelude:

* [`store.dhall-lang.org` - `Prelude-v21.1.0/Natural/greaterThan.dhall`](https://store.dhall-lang.org/Prelude-v21.1.0/Natural/greaterThan.dhall.html#var2-5)

That has the following dependency tree:

```
https://prelude.dhall-lang.org/Natural/greaterThan.dhall
└ ./lessThan.dhall
  ├ ../Bool/not.dhall
  └ ./greaterThanEqual.dhall
    └ ./lessThanEqual.dhall
```

… but you can eliminate all of the indirection by normalizing the expression:

```haskell
⊢ https://prelude.dhall-lang.org/Natural/greaterThan.dhall


λ(x : Natural) → λ(y : Natural) → Natural/isZero (Natural/subtract y x) == False
```

# Generalizing evaluation

Morte and Dhall share the following features:

* They are both interpreters
* They are both strongly normalizing
* They both support pretty-printing arbitrary syntax trees

If you combine those three features, you can "evaluate" an expression by:

* Normalizing the expression
* Pretty-printing the normalized expression

Carefully note that you can evaluate and display arbitrary expressions

You're not limited to evaluating expressions that return plain data

I use the term "normalization" to refer to this generalized evaluation

# Normalization example 

Here are some examples of how normalization is more powerful than evaluation:

```haskell
⊢ :let sum = https://prelude.dhall-lang.org/Natural/sum.dhall

⊢ λ(x : Natural) → λ(y : Natural) → sum [ x, y ]  -- You can inline code

λ(x : Natural) → λ(y : Natural) → x + y
```

```haskell
⊢ λ(x : Bool) → if x then True else False  -- You can do symbolic simplification

λ(x : Bool) → x
```

```haskell
⊢ :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| :let Map =
|        λ(args : { key : Type, value : Type }) →
|          List { mapKey : args.key, mapValue : args.value }
| 

⊢ Map { key = Text, value = Bool }  -- You can normalize types

List { mapKey : Text, mapValue : Bool }
```

# Overview

# Universal quantification

Morte and Dhall also support polymorphism (a.k.a. "universal quantification")

However, type abstraction and type application are explicit (unlike Haskell)

For example, here how you define and use the identity function in Dhall:

```haskell
⊢ :let id = λ(a : Type) → λ(x : a) → x

id : ∀(a : Type) → ∀(x : a) → a

⊢ id Text "Hello"

"Hello"
```

Why does the inferred type of `id` have two "forall" symbols (`∀`), though?

# Explicit type abstraction / application

Let's compare the Dhall type:

```haskell
id : ∀(a : Type) → ∀(x : a) → a
```

… and the equivalent Haskell type (with `ExplicitForAll` enabled):

```haskell
id :: forall a . a -> a
```

In Dhall, `id` is a function of two arguments (unlike Haskell):

* The first argument, `a` is a `Type`
* The second argument, `x` has a type that matches the first argument

We can "specialize" `id` by supplying only the first argument:

```haskell
⊢ id Text  -- Specialization is also a special case of normalization

λ(x : Text) → x
```

This is the same thing as `TypeApplications` in Haskell (e.g. `id @Text`)

# Π types

The `forall` / `∀` keyword  in Dhall actually denotes a "Π type"

All function types in Dhall are actually Π types of the form:

```haskell
∀(a : A) → B
```

… and you can think of this as a function type where you can name the input:

* `a` is the name of the function input
* `A` is the type of the function input
* `B` is the type of the function output

Dhall provides syntactic sugar where this function type:

```haskell
A → B
```

… is actually a shorthand for a Π type that ignores the input name:

```haskell
∀(_ : A) → B
```

# Naming function arguments

You can also use Π types to name function arguments that aren't types

For example, the `Text/replace` built-in's type names the function arguments:

```haskell
⊢ :type Text/replace

∀(needle : Text) → ∀(replacement : Text) → ∀(haystack : Text) → Text
```

Names like these are irrelevant; nothing happens if you change or omit them

The name is only relevant if it appears "downstream" in the function type

```haskell
--     This name is relevant because `a` shows up downstream within the type
--     ↓
id : ∀(a : Type) → ∀(x : a) → a
--                   ↑
--                   This name is irrelevant, because `x` is not used downstream
```

# Universal quantification - revisited

Universal quantification is a special case of a Π type where `A = Type`

That means that the following Haskell type:

```haskell
forall a . a -> a
```

… is the same as this Dhall type:

```haskell
∀(a : Type) → a → a
```

… which is syntactic sugar for this type:

```haskell
∀(a : Type) → ∀(_ : a) → a
```

… which is the same type as this type:

```haskell
∀(a : Type) → ∀(x : a) → a
```

… because the second input name is irrelevant

# Overview

# Name preservation

Strong normalization and Π types benefit from "name preservation". Specifically:

* Normalization preserves variable names as much as possible<sup>†</sup>
* Inferred function types (Π types) preserve variable names as much as possible

<sup>†</sup> Dhall does not yet preserve variable names for imports protected by
  integrity checks.  See: [#1185](https://github.com/dhall-lang/dhall-lang/issues/1185)

# Normalization preserves names

Here are some examples of how normalization preserves names

```haskell
⊢ λ(x : Bool) → x  -- Phew!  That was easy 😌

λ(x : Bool) → x
```

```haskell
⊢ λ(x : Bool) → (λ(y : Bool) → λ(z : Bool) → y) x  -- Still straightforward

λ(x : Bool) → λ(z : Bool) → x
```

```haskell
⊢ :let compose = λ(f : Bool → Bool) → λ(g : Bool → Bool) → λ(x : Bool) → f (g x) 

compose : ∀(f : Bool → Bool) → ∀(g : Bool → Bool) → ∀(x : Bool) → Bool

⊢ compose (λ(y : Bool) → y) (λ(z : Bool) → z)  -- Tiny bit trickier

λ(x : Bool) → x
```

# Type inference preserves names

Here are some examples of how type inference preserves names

```haskell
⊢ :type λ(x : Bool) → x  -- Irrelevant names are preserved by default

∀(x : Bool) → Bool
```

You can also change or omit inferred names with a type annotation:

```haskell
⊢ :type (let f : Bool → Bool = λ(x : Bool) → x in f)

Bool → Bool
```

```haskell
⊢ :type Text/replace

∀(needle : Text) → ∀(replacement : Text) → ∀(haystack : Text) → Text

⊢ :type λ(x : Text) → Text/replace x  -- η-expansion can change inferred names

∀(x : Text) → ∀(replacement : Text) → ∀(haystack : Text) → Text

⊢ :type λ(_ : Text) → Text/replace _  -- _ is a valid variable name

Text → ∀(replacement : Text) → ∀(haystack : Text) → Text
```

# The hard part

What should the interpreter return as the normal form for this expression?

```haskell
λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x
```

. . .

I'll give you a head start:

```haskell
λ(? : Bool) → λ(? : Text) → ?
```

# Name capture

I'll start off with the wrong answer.  If you normalize this:

```haskell
λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x
```

… you should **NOT** get this:

```haskell
λ(x : Bool) → λ(x : Text) → x
```

That last `x` refers to the second (`Text`) argument, but the correct answer
should return the first (`Bool`) argument

This is a common type of implementation error known as "name capture"
 
For example, the first draft of Morte made exactly this mistake:
 
* [Gabriel439/Morte - Issue #1](https://github.com/Gabriel439/Haskell-Morte-Library/issues/1)

… and the discussion on that issue is where I devised this trick

# Capture-avoiding substitution

A "capture-avoiding" substitution algorithm does not have "name capture" bugs

Capture-avoiding substitution algorithms typically fall into two categories:

* Named representations

  This approach preserves variable names, but adds some unique suffix when
  name capture is detected

* Nameless representations (i.e. De Bruijn indices)

  This approach replace variable names with integers

# De Bruijn indices

For example, using De Bruijn indices our pathological expression:

```haskell
λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x
```

… would normalize to:

```haskell
λ → λ → @1
```

… where `@n` denotes a De Bruijn index of `n`

The bug is gone, but now our original names are gone, too! 😔

# Renaming variables

Another solution is add a unique suffix variables when their names collide

For example, such an implementation might normalize our pathological expression:

```haskell
λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x
```

… to:

```haskell
λ(x1 : Bool) → λ(x : Bool) → x1
```

I call this solution "name mangling"

# Name mangling in GHC

We can see what GHC does by saving our function to a Haskell file:

```haskell
module Example where

f :: Bool -> String -> Bool
f = \x -> (\y x -> y) x
```

… and asking GHC to dump an intermediate representation:

```haskell
$ ghc -O2 -ddump-simpl -dsuppress-all Example.hs
…
f = \ x_atz _ -> x_atz
```

`ghc` did two things to avoid the name collision:

* rename the second variable to `_` since it's unused
* add a unique suffix to `x` to avoid collision

Even if GHC could pretty-print arbitrary expressions, the names would be mangled

# Mangling types

GHC doesn't support generalized evaluation, so name mangling is not as visible

**BUT**, name mangling still affects the user experience in another way: types!

Name mangling shows up in error messages:

```haskell
>>> :type ([] .)

<interactive>:1:2: error:
    • Couldn't match expected type ‘b -> c’ with actual type ‘[a0]’
    • In the first argument of ‘(.)’, namely ‘[]’
      In the expression: ([] .)
```

… and inferred types:

```
>>> :type (const .)
(const .) :: (a1 -> a2) -> a1 -> b -> a2
```

We get used to this, but polluting types with irrelevant numbers is jarring!

# Spoiler

Okay, but what does Dhall *actually* return for our pathological example?

```haskell
⊢ λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x

λ(x : Bool) → λ(x : Text) → x@1
```

Dhall uses a representation that mixes a named and nameless representation

The upcoming sections will explain that in more detail

# Overview

# Named representation

I will start with an untyped lambda calculus for this part to simplify things:

```haskell
data Syntax
    = Variable String
    | Lambda String Syntax
    | Apply Syntax Syntax
```

The above syntax tree is a "named" representation

For example, we would represent this lambda expression:

```haskell
λf → λx → f x
```

… as this `Syntax` tree:

```haskell
Lambda "f" (Lambda "x" (Apply (Variable "f") (Variable "x")))
```

# Nameless representation

The corresponding nameless representation is:

```haskell
data Syntax
    = Variable Int
    | Lambda Syntax
    | Apply Syntax Syntax
```

For example, we would represent this lambda expression:

```haskell
λf → λx → f x
```

… as this `Syntax` tree:

```haskell
Lambda (Lambda (Apply (Variable 1) (Variable 0)))
```

… as if the user had written:

```haskell
λ → λ → @1 @0
```

I prefix the indices with `@` to avoid confusing them with ordinary numbers

# De Bruijn indices

A [De Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index) replaces a
variable name with a number

The number counts how far away the matching lambda is:

```haskell
    ┌───────┐
    ↓       │
λ → λ → @1 @0
↑        │
└────────┘
```

This variable numbering convention has the following nice properties:

* index assignment is context free
* this convention biases towards low indices (especially `0`)
* the capture-avoiding substitution algorithm is simple

# Comparing named and nameless

Let's compare and contrast the two representations:

```haskell
data Syntax                 │  data Syntax
    = Variable String       │      = Variable Int
    | Lambda String Syntax  │      | Lambda Syntax
    | Apply Syntax Syntax   │      | Apply Syntax Syntax
```

The nameless representation:

* Uses `Int` instead of `String` in the `Variable` constructor

* Omits the variable name for `Lambda`

  In fact, there is no unique "name" (index) that corresponds to that lambda

To illustrate the latter point:

```haskell
┌────┐  ┌────┐
↓    │  ↓    │
λ → @0 (λ → @0 @1)
↑               │
└───────────────┘
```

# The trick

I could summarize the entire presentation with this meme:

![](./porque.jpg)

Translation: "Why not both?"

We can get the best of both worlds by mixing a named and nameless representation

However, there are multiple ways to do this and I have a specific way in mind

# Namespaced De Bruijn indices - Part 1

The first part of the trick is to use this syntax tree:

```haskell
data Syntax
    = Variable String Int
    | Lambda String Syntax
    | Apply Syntax Syntax
```

… which is a hybrid between the two representations we presented so far:

```haskell
data Syntax                 │  data Syntax
    = Variable String       │      = Variable Int
    | Lambda String Syntax  │      | Lambda Syntax
    | Apply Syntax Syntax   │      | Apply Syntax Syntax
```

This is like the named representation, except every variable has a De Bruijn
index

I call this representation "Namespaced De Bruijn indices"

# Example 0

The easiest way to explain this representation is with an example

The following expression:

```haskell
λx → λy → λx → x@0
```

… corresponds to this syntax tree:

```haskell
Lambda "x" (Lambda "y" (Lambda "x" (Variable "x" 0)))
```

… and it represents a curried function that returns the third argument:

```haskell
           ┌───┐
           ↓   │
λx → λy → λx → x@0
```

This is pretty straightforward

# Example 1

Now let's try a slightly more interesting example

The following expression:

```haskell
λx → λy → λx → y@0
```

… corresponds to this syntax tree:

```haskell
Lambda "x" (Lambda "y" (Lambda "x" (Variable "y" 0)))
```

… which returns the second function argument:

```haskell
      ┌────────┐
      ↓        │
λx → λy → λx → y@0
```

… because that is the innermost bound variable named `y`

Note: the De Bruijn index is `0`, but we ignore bound variables other than `y`

You can think of the index as being "namespaced" only to variables named `y`

# Example 2

Now let's try an even more interesting example

The following expression:

```haskell
λx → λy → λx → x@1
```

… corresponds to this syntax tree:

```haskell
Lambda "x" (Lambda "y" (Lambda "x" (Variable "x" 1)))
```

… which returns the first function argument:

```haskell
 ┌─────────────┐
 ↓             │
λx → λy → λx → x@1
```

… because that is the second-innermost bound variable named `x`

Like to the last example, the index is namespaced to variables named `x`

This means that users can refer to shadowed variables using a non-zero index!

# Namespaced De Bruijn indices - Part 2

The second part of the trick is to add syntactic sugar for eliding 0 indices

In other words, this code:

```haskell
λx → (λy → λx → y) x
```

… is syntactic sugar for this more explicit form:

```haskell
λx → (λy → λx → y@0) x@0
```

… and both of them parse to the same syntax tree, which is:

```haskell
Lambda "x" (Apply (Lambda "y" (Lambda "x" (Variable "y" 0))) (Variable "x" 0))
```

This syntactic sugar also works in reverse when pretty-printing expressions

If you were to pretty-print the above syntax tree you would get:

```haskell
λx → (λy → λx → y) x
```

… because the pretty-printer elides 0 indices

# Examples - Revisited

We can simplify this:

```haskell
λx → λy → λx → x@0
```

… to this:

```haskell
λx → λy → λx → x
```

Similarly, we can simplify this:

```haskell
λx → λy → λx → y@0
```

… to this:

```haskell
λx → λy → λx → y
```

But we cannot simplify this:

```haskell
λx → λy → λx → x@1
```

# Emergent properties

This trick is neat because the syntactic sugar makes indices unintrusive

In practice, users don't even know indices exist (Case in point: Dhall)

The reason why is because non-zero indices only arise in two cases:

* The user wishes to explicitly reference a shadowed variable (which is rare)

  … such as our last example:

  ```haskell
  λx → λy → λx → x@1
  ```

* The indices appear in a β-reduced result (also rare)

  For example, our pathological input has no visible indices:

  ```haskell
  λx → (λy → λx → y) x
  ```

  … but the β-reduced result requires a visible index:

  ```haskell
  λx → λx → x@1
  ```

# TODO:

* Show how you can give a nameless type to a polymorphic function like
  `List/length`

# α-equivalence

De Bruijn indices also permit a straightforward "α-equivalence" check

Two expressions are "α-equivalent" if they are the same up to renaming variables

You can determine α-equivalence by comparing nameless representations

For example, this expression:

```haskell
λx → x
```

… and this expression:

```haskell
λy → y
```

… are α-equivalent because they both share the same nameless representation:

```haskell
λ → @0
```

[dhall]: https://dhall-lang.org
