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

Morte and Dhall also support polymorphism, but it's explicit

For example, here how you define and use the identity function in Dhall:

```haskell
⊢ :let id = λ(a : Type) → λ(x : a) → x

id : ∀(a : Type) → ∀(x : a) → a

⊢ id Text "Hello"

"Hello"
```

Polymorphism is also known as "universal quantification"

# Π types generalize universal quantification

Why does the inferred type of `id` have two "forall"s (`∀`), though?

```haskell
--   ↓             ↓
id : ∀(a : Type) → ∀(x : a) → a
```

# Name preservation

Both Morte and Dhall preserve variable names when normalizing code<sup>†</sup>

Here are some non-trivial examples:

```haskell
⊢ (λ(f : Bool → Bool) → λ(x : Bool) → f (f x)) (λ(y : Bool) → y)

λ(x : Bool) → x
```

```haskell
⊢ λ(x : Bool) → (λ(y : Bool) → λ(z : Bool) → y) x

λ(x : Bool) → λ(z : Bool) → x
```

<sup>†</sup> Dhall does not yet preserve variable names for imports protected by
  integrity checks.  See: [#1185](https://github.com/dhall-lang/dhall-lang/issues/1185)

Equally important, types preserve inferred

# The bug

However, the first draft of Morte had a broken implementation of β-reduction!

* [Gabriel439/Morte - Issue #1 - Substitution with De Bruijn indices](https://github.com/Gabriel439/Haskell-Morte-Library/issues/1)


[dhall]: https://dhall-lang.org
