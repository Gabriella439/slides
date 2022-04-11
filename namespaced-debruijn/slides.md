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

* β-reduction
* Π types
* Name preservation
* Nameless and named representations
* Comparison to existing approaches

# History

I first stumbled upon this idea while working on
[Morte](https://github.com/Gabriel439/Haskell-Morte-Library/issues/1), which is:

* essentially the calculus of constructions + an import system
* a proof of concept of my [internet of code](https://www.haskellforall.com/2015/05/the-internet-of-code.html) idea
* the predecessor of [Dhall][dhall]

Also, I name all of my languages after characters from [Planescape: Torment](https://en.wikipedia.org/wiki/Planescape:_Torment)

# Motivation

Both Morte and Dhall are
[total functional programming languages](https://en.wikipedia.org/wiki/Total_functional_programming)

This means all expressions can be "β-reduced" to a normal form.  For example:

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

# β-reduction

β-reduction is (sort of) the same thing as function evaluation

The Dhall REPL β-reduces every expression you enter in the REPL:

```haskell
⊢ Natural/even 2  -- This is a "β-reducible expression" (a.k.a. a "β-redex")

True
```

The key thing to stress is that β-reduction even evaluates "under lambda"

```haskell
⊢ λ(a : Bool) → (λ(b : Bool) → [ b, b ]) a

λ(a : Bool) → [ a, a ]
```

This is why β-reduction can interpret incomplete code (like `List/generate 10`)

# β-reduction can improve code comprehension

β-reduction eliminates indirection

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

… but you can eliminate all of the indirection by β-reducing the expression<sup>†</sup>:

```haskell
⊢ https://prelude.dhall-lang.org/Natural/greaterThan.dhall


λ(x : Natural) → λ(y : Natural) → Natural/isZero (Natural/subtract y x) == False
```

<sup>†</sup> Technically, you also have to resolve imports before β-reducing the expression

# Generalizing evaluation

Morte and Dhall both "evaluate" an expression by:

* β-reducing the expression
* Pretty-printing the β-reduced expression

Carefully note that this "generalized evaluation" works for arbitrary
expressions

You're not limited to evaluating expressions that return plain/inert data

# More β-reduction examples

Here are some more examples of how powerful β-reduction is:

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

⊢ Map { key = Text, value = Bool }  -- You can β-reduce types

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
⊢ id Text  -- Specialization is also a special case of β-reduction

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

β-reduction and Π types benefit from "name preservation". Specifically:

* β-reduction preserves variable names as much as possible<sup>†</sup>
* Inferred function types (Π types) preserve variable names as much as possible

<sup>†</sup> Dhall does not yet preserve variable names for imports protected by
  integrity checks.  See: [#1185](https://github.com/dhall-lang/dhall-lang/issues/1185)

# β-reduction preserves names

Here are some examples of how β-reduction preserves names

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
⊢ :type λ(x : Bool) → x  -- Irrelevant names are still preserved

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

I'll start off with the wrong answer.  If you β-reduce this:

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

  This approach preserves variable names, but adds some unique suffix to avoid
  name capture

* Nameless representations (i.e. De Bruijn indices)

  This approach replace variable names with integers

# De Bruijn indices

For example, using De Bruijn indices our pathological expression:

```haskell
λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x
```

… would β-reduce to:

```haskell
λ → λ → @1
```

… where `@n` denotes a De Bruijn index of `n`

The bug is gone, but now our original names are gone, too! 😔

# Renaming variables

Another solution is add a unique suffix variables when their names collide

For example, such an implementation might β-reduce our pathological expression:

```haskell
λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x
```

… to:

```haskell
λ(x : Bool) → λ(x1 : Bool) → x
```

Henceforth, I will call this solution "name mangling"

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

I won't present the substitution algorithm for this approach (it's inelegant)

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

# Substitution algorithm for De Bruijn indices

This is adapted from Pierce's "Types and Programming Languages" book:

```
k[j ≔ s]        =  | s  if j = k
                   | k  otherwise
(λ → t)[j ≔ s]  =  λ → t[j+1 ≔ ↑(1,0)s]
(t u)[j ≔ s]    =  (t[j ≔ s] u[j ≔ s])

↑(d,j)k        =  | k+d  if j ≤ k
                  | k    otherwise
↑(d,j)(λ → t)  =  λ → ↑(d,j+1)t
↑(d,j)(t u)    =  (↑(d,j)t ↑(d,j)u)
```

I won't walk through this but I want people to appreciate the following:

* The algorithm is concise
* The algorithm doesn't require any sort of name supply or name mangling

I will include a Haskell algorithm at the end of this talk that's not so terse

# α-reduction

Now that we've introduced De Bruijn indices we can define "α-reduction":

> α-reduction converts an expression to the equivalent representation using De
> Bruijn indices

I've never seen this definition in the wild, but this is the "obvious"
definition

# α-equivalence

The reason I call this α-reduction is because you can define α-equivalence as
follows:

> Two terms are α-equivalent if their α-reduced forms are identical

For example, this expression:

```haskell
λx → x
```

… and this expression:

```haskell
λy → y
```

… are α-equivalent because they both share the same representation using De
Bruijn indices:

```haskell
λ → @0
```

This parallels the way β-equivalence is defined:

> Two terms are β-equivalent if their β-reduced forms are identical

… and that's why the definition I gave for α-reduction is the "obvious" one

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

Like the last example, the index is namespaced to variables named `x`

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

# Namespaced indices in practice

This trick is neat because the syntactic sugar makes indices unintrusive

In practice, most users don't even know indices exist (Case in point: Dhall)

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

# Substitution algorithm for Namespaced De Bruijn indices

Here's the original substution algorithm for De Bruijn indices:

```
k[j ≔ s]        =  | s  if j = k
                   | k  otherwise
(λ → t)[j ≔ s]  =  λ → t[j+1 ≔ ↑(1,0)s]
(t u)[j ≔ s]    =  (t[j ≔ s] u[j ≔ s])

↑(d,j)k        =  | k+d  if j ≤ k
                  | k    otherwise
↑(d,j)(λ → t)  =  λ → ↑(d,j+1)t
↑(d,j)(t u)    =  (↑(d,j)t ↑(d,j)u)
```

… and here's how you update the algorithm for namespaced De Bruijn indices:

```
y@k[x@j ≔ s]       =  | s  if x = y && j = k
                      | k  otherwise
(λy → t)[x@j ≔ s]  =  | λy → t[x@(j+1) ≔ ↑(1,x@0)s]  if x = y
                      | λy → t[x@j     ≔         s]  otherwise
(t u)[x@j ≔ s]     =  (t[x@j ≔ s] u[x@j ≔ s])

↑(d,x@j)y@k       =  | y@(k+d)  if x = y && j ≤ k
                     | y@k      otherwise
↑(d,x@j)(λy → t)  =  | λy → ↑(d,x@(j+1))t  if x = y
                     | λy → ↑(d,x@j    )t  otherwise
↑(d,x@j)(t u)     =  (↑(d,x@j)t ↑(d,x@j)u)
```

# Don't get hung up on the implementation

I want to stress that the implementation is not the important part here

There are more efficient solutions than the algorithm on the previous slide<sup>†</sup>

The key thing to take away from this talk is the **desired user experience**:

* We should let input code reference shadowed variables
* We should let output results reference shadowed variables

If we satisfy those requirements we benefit from the nice emergent properties

<sup>†</sup> Check out this project for a more efficient implementation you can fork:

* [GitHub - Fall-from-Grace](https://github.com/Gabriel439/grace)

# Variable shadowing - Bug or feature?

"Should we provide userland support for referencing shadowed variables?"  **YES!**

Consider this example code, which references a shadowed variable using an index:

```haskell
⊢ :let f = λ(x : Bool) → λ(x : Text) → x@1

f : ∀(x : Bool) → ∀(x : Text) → Bool
```

If we η-expand that expression, the index disappears!

```haskell
⊢ λ(a : Bool) → f a

λ(a : Bool) → λ(x : Text) → a
```

Indices intelligently revert to zero when they're no longer needed

What if you forgo language support in favor of mangling the name in userland?

```haskell
⊢ :let f = λ(x : Bool) → λ(x1 : Text) → x

f : ∀(x : Bool) → ∀(x1 : Text) → Bool
```

Now the name is irreversibly scarred even after η-expansion:

```haskell
⊢ λ(a : Bool) → f a

λ(a : Bool) → λ(x1 : Text) → a
```

# Overview

# Comparison to name mangling - Part 1

"Aren't namespaced De Bruijn indices just another form of name mangling?" **NO!**

Consider this expression, which η-expands our pathological example:

```haskell
⊢ λ(a : Bool) → (λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x) a

λ(a : Bool) → λ(x : Text) → a
```

Internally there is a name collision, but the final result requires no indices

A name mangling approach would get this wrong and produce something like:

```haskell
λ(a : Bool) → λ(x1 : Text) → a
```

Name mangling irreversibly scars variable names when avoiding name capture

Namespaced De Bruijn indices revert back to 0 when shadowing disappears

```haskell
⊢ λ(a : Bool) → (λ(x : Bool) → λ(x : Text) → x@1) a

λ(a : Bool) → λ(x : Text) → a
```

# Comparison to name mangling - Part 2

This approach improves upon the type-level UX, too!

The inferred type of our pathological expression has no mangled names at all:

```haskell
⊢ :type λ(x : Bool) → (λ(y : Bool) → λ(x : Text) → y) x

∀(x : Bool) → ∀(x : Text) → Bool
```

Had we used name mangling then we would have gotten an inferred type like:

```haskell
∀(x : Bool) → ∀(x1 : Text) → Bool
```

# Comparison to De Bruijn indices

Namespaced De Bruijn indices generalize traditional De Bruijn indices

You "α-reduce" namespaced DeBruijn indices by renaming all variables to `_`:

For example this, expression:

```haskell
λx → λy → x
```

… α-reduces to:

```haskell
λ_ → λ_ → _@1
```

The result is still using our namespaced De Bruijn index representation, though:

```haskell
Lambda "_" (Lambda "_" (Variable "_" 1))
```

We don't need separate syntax trees for our named and nameless representation!

# Conclusion

Namespaced De Bruijn indices combine the best of both worlds:

* We preserve names like a traditional named representation
* We get a mangling-free substitution algorithm like De Bruijn indices

This means that:

* We can pretty-print arbitrary β-reduced expressions
* We also get language support for referencing shadowed variables

This trick is most appropriate for languages that pretty-print β-reduced
expressions

This trick is useful for other interpreted languages, if only to simplify their
implementation

This approach has been vetted extensively in the wild via Dhall

[dhall]: https://dhall-lang.org
