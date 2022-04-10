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
* how [Dhall][dhall] uses this trick to great effect

Also, as the blog post notes,
[CINNI](https://www.sciencedirect.com/science/article/pii/S1571066105801252)
predates this work and presents exactly the same idea.

# Overview

* Strong normalization
* Π types
* Name preservation

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

… because the second input name is irrelevant.

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
λ(x : Bool) → let y = x in λ(x : Text) → y
```

. . .

I'll give you a head start:

```haskell
λ(? : Bool) → λ(? : Text) → ?
```

# Name capture

I'll start off with the wrong answer.  If you normalize this:

```haskell
λ(x : Bool) → let y = x in λ(x : Text) → y
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

… and the discussion on that issue is where I devised this trick.

# Capture-avoiding substitution

A "capture-avoiding" substitution algorithm does not have "name capture" bugs

One such algorithm is a "nameless" representation (a.k.a. De Bruijn indices)

Capture-avoiding substitution algorithms typically fall into two categories:

* Nameless representations

  a.k.a. De Bruijn indices

  This approach replace variable names with integers

* Named representations

  This approach preserves variable names, but adds some unique suffix when
  name capture is detected

# De Bruijn indices

For example, using De Bruijn indices our pathological expression:

```haskell
λ(x : Bool) → let y = x in λ(x : Text) → y
```

… would normalize to:

```haskell
λ → λ → @1
```

… where `@n` denotes a De Bruijn index of `n`.

The bug is gone, but now our original names are gone, too! 😔

# Renaming variables

Another solution is rename variables to make them sufficiently unique.

We can see what GHC does by saving our function to a Haskell file:

```haskell
module Example where

f :: Bool -> String -> Bool
f x = let y = x in \x -> y
```

… and asking GHC to dump an intermediate representation:

```haskell
$ ghc -O2 -ddump-simpl -dsuppress-all Example.hs
…
f = \ x_atA _ -> x_atA
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

We eventually get used to this, but polluting types with numbers is jarring!

[dhall]: https://dhall-lang.org

# TODO:

* Show how you can give a nameless type to a polymorphic function like
  `List/length`
