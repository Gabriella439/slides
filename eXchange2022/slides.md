% Fall-from-Grace: A Typed and Programmable Superset of JSON
% Gabriella Gonzalez
% December 9, 2022

## About me

Hi, I'm Gabby and I ❤️ "PLT crimes"

I love building languages that break unspoken rules of PLT

<style>
.reveal h1, .reveal h2, .reveal h3, .reveal h4, .reveal h5 {
  text-transform: none;
}

.reveal p {
  text-align: left;
}

.reveal ul {
  display: block;
}

.reveal ol {
  display: block;
}

section {
  overflow-y: auto !important;
  overflow-x: hidden !important;
  height: 100%;
}

code {
  max-height: none !important;
}
</style>

## Background

Fall-from-Grace was inspired by a prior talk of mine:

[How to market Haskell to a mainstream programmer](https://www.youtube.com/watch?v=fNpsgTIpODA)

Summary: Programming languages are Haskell's path to mainstream success

## Haskell + PLT = ❤️

This is already happening:

- Cybersecurity
- Finance / Blockchain
- Open source languages:
  - PureScript
  - Elm
  - Unison
  - Agda
  - Dhall

## Haskell's origin story

Haskell originated as a research vehicle for PLT:

[A History of Haskell: Being Lazy with Class](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf)

This "founder effect" primed Haskell to be well-suited for implementing
programming languages

## The missing pieces 🧩

There are three things missing for Haskell to go viral:

- Clean reference implementations of programming languages

  ⭐️ That is this talk ⭐️

- Corresponding books to go with these reference implementations

- Functional logic programming DSL for type-checking

  - Perhaps Verse 😝

# Outline

- <span style="color:#ff2c2d">Introduction to Fall-from-Grace</span>
- Language design
- The Grace Browser

## Fall-from-Grace

Fall-from-Grace ("Grace" for short) is an interpreted and functional programming
language

```bash
$ nix run github:Gabriella439/grace -- repl
```
```haskell
>>> 2 + 2
4

>>> :let twice = \x -> [ x, x ]

>>> twice (twice true)
[ [ true, true ], [ true, true ] ]

>>> :type twice
forall (a : Type) . a -> List a
```

## Build your own language

Grace's primary goal is to be an easy-to-fork language

Easy-to-fork means:

- High quality implementation (in **HASKELL**)
- Clear code (as best as I can manage)
- Clear instructions for extending the language

Now y'all can stop making Dhall into something it's not 🙂

## Headline features

Grace has a unique combination of features:

- Grace is a superset of JSON

- Grace is typed (with type inference)

- Grace is a programming language

These features are particularly useful for configuration languages

## Feature matrix

| | Cue | Jsonnet | Dhall | Grace |
|-|-|-|-|-|
| JSON-compatible | ☑ | ☑ | ☐ | ☑ |
| Typed           | ☑ | ☐ | ☑ | ☑ |
| Programmable    | ☐ | ☑ | ☑ | ☑ |

## JSON-compatible syntax

```json
{
  "clients": [
    {
      "isActive": true,
      "age": 36,
      "name": "Dunlap Hubbard",
      "email": "dunlaphubbard@cedward.com",
      "phone": "+1 (890) 543-2508"
    },
    {
      "isActive": true,
      "age": 24,
      "name": "Kirsten Sellers",
      "email": "kirstensellers@emergent.com",
      "phone": "+1 (831) 564-2190"
    }
  ]
}
```

## Improvements over JSON

- Comments!!!

  ```json
  # Grace uses Python-style comments
  [ true, false ]
  ```

- You can omit quotes for field names without spaces or punctuation

   ```haskell
   { x: 1.1, y: -2.1 }
   ```

## Types

Here is the inferred type of that program:

```haskell
{ clients:
    List
      { isActive: Bool
      , age: Natural
      , name: Text
      , email: Text
      , phone: Text
      }
}
```

## Comparison to JSON Schema

```json
{
  "type": "object",
  "properties": {
    "clients": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "isActive": { "type": "boolean" },
          "age": { "type": "number", "minimum": 0 },
          "name": { "type": "string" },
          "email": { "type": "string" },
          "phone": { "type": "string" }
        },
        "required": [
          "isActive",
          "age",
          "name",
          "email",
          "phone"
        ]
      }
    }
  },
  "required": [ "clients" ]
}
```

## Strong typing

Grace is "strongly-typed" by default:

```haskell
[ 1, true ]                                 # Rejected

[ 1, true ] : JSON                          # Accepted

[ 1, true ] : List JSON                     # Accepted

[ 1, true ] : List (exists (a : Type) . a)  # Accepted
```

"Weak typing" still preserves type safety

## Programmable

Here is a Grace program that uses a helper function:

```haskell
let makeUser = \user ->
      let home       = "/home/" + user
      let privateKey = home + "/.ssh/id_ed25519"
      let publicKey  = privateKey + ".pub"
      in  { home: home
          , privateKey: privateKey
          , publicKey: publicKey
          }

in  [ makeUser "bill"
    , makeUser "jane"
    ]
```

## Programmable

Here is the result (which is valid JSON):

```json
[ { "home": "/home/bill"
  , "privateKey": "/home/bill/.ssh/id_ed25519"
  , "publicKey": "/home/bill/.ssh/id_ed25519.pub"
  }
, { "home": "/home/jane"
  , "privateKey": "/home/jane/.ssh/id_ed25519"
  , "publicKey": "/home/jane/.ssh/id_ed25519.pub"
  }
]
```

## Imports - data

You can import expressions by their relative paths:

```haskell
[ ./cake.ffg
, ./raised.ffg
, ./old-fashioned.ffg
]
```

```haskell
# ./cake.ffg
{ name: "Cake donut"
, batters: [ "Regular", "Chocolate", "Blueberry", "Devil's Food" ]
, topping: [ "None"
           , "Glazed"
           , "Sugar"
           , "Powdered Sugar"
           , "Chocolate with Sprinkles"
           , "Chocolate"
           , "Maple"
           ]
}
```

```haskell
# ./raised.ffg
{ name: "Raised donut"
, batters: [ "Regular" ]
, topping: [ "None", "Glazed", "Sugar", "Chocolate", "Maple" ]
}
```

```haskell
# ./old-fashioned.ffg
{ name: "Old Fashioned donut"
, batters: [ "Regular", "Chocolate" ]
, topping: [ "None", "Glazed", "Chocolate", "Maple" ]
}
```

## Imports - data

```json
[ { "name": "Cake donut"
  , "batters": [ "Regular", "Chocolate", "Blueberry", "Devil's Food" ]
  , "topping": [ "None"
               , "Glazed"
               , "Sugar"
               , "Powdered Sugar"
               , "Chocolate with Sprinkles"
               , "Chocolate"
               , "Maple"
               ]
  }
, { "name": "Raised donut"
  , "batters": [ "Regular" ]
  , "topping": [ "None", "Glazed", "Sugar", "Chocolate", "Maple" ]
  }
, { "name": "Old Fashioned donut"
  , "batters": [ "Regular", "Chocolate" ]
  , "topping": [ "None", "Glazed", "Chocolate", "Maple" ]
  }
]
```

## Imports - functions

You can import any valid Grace expression, including functions:

```haskell
# ./makeUser.ffg
\user ->
    let home       = "/home/" + user
    let privateKey = home + "/.ssh/id_ed25519"
    let publicKey  = privateKey + ".pub"
    in  { home: home
        , privateKey: privateKey
        , publicKey: publicKey
        }
```

```haskell
let makeUser = ./makeUser.ffg

in  [ makeUser "bill"
    , makeUser "jane"
    ]
```

… or equivalently:

```haskell
[ ./makeUser.ffg "bill"
, ./makeUser.ffg "jane"
]
```

## Imports - URLs

We can import URLs, like [this gist](https://gist.githubusercontent.com/Gabriella439/712d0648bbdcfcc83eadd0ee394beed3/raw/1b03f661577521b4d3dc6ca73dd11475a30c1594/incomeTax.ffg)

```haskell
let incomeTax = https://gist.githubusercontent.com/Gabriella439/712d0648bbdcfcc83eadd0ee394beed3/raw/1b03f661577521b4d3dc6ca73dd11475a30c1594/incomeTax.ffg

in  incomeTax
      { "Filing status": 'Married, filing jointly'{ }
      , "Taxable income": 200000
      }
```

```json
{ "Tax": 35671.0 }
```

## Weakly-typed JSON

You can import weird JSON with a type annotation:

```bash
$ cat ./weird.json
```
```json
[ 1, true ]
```

```haskell
./weird.json : JSON
```

This way you don't have to modify the JSON

## Anonymous records

```haskell
>>> :let gabby = { name: "Gabriella Gonzalez" }

>>> :let john = { name: "John Doe", age: 24 }

>>> gabby.name
"Gabriella Gonzalez"

>>> john.name
"John Doe"
```

## Row polymorphism

```haskell
>>> :let getName = \record -> record.name

>>> :type getName
forall (a : Type) (b : Fields) . { name: a, b } -> a

>>> getName gabby
"Gabriella Gonzalez"

>>> getName john
"John Doe"
```

Grace does **NOT** (yet) support record concatenation!

## Sum types

Constructors are identifiers in uppercase or single quotes

```haskell
 # ./packages.ffg
[ GitHub
    { repository: "https://github.com/Gabriel439/Haskell-Turtle-Library"
    , revision: "ae5edf227b515b34c1cb6c89d9c58ea0eece12d5"
    }
, Local { path: "~/proj/optparse-applicative" }
, Local { path: "~/proj/discrimination" }
, Hackage { name: "lens", version: "4.15.4" }
, GitHub
    { repository: "https://github.com/haskell/text"
    , revision: "ccbfabedea1cf5b38ff19f37549feaf01225e537"
    }
, Local { path: "~/proj/servant-swagger" }
, Hackage { name: "aeson", version: "1.2.3.0" }
]
```

## Polymorphic variants

Inferred type is an "open sum":

```haskell
forall (a : Alternatives) .
  List
    < GitHub: { repository: Text, revision: Text }
    | Local: { path: Text }
    | Hackage: { name: Text, version: Text }
    | a
    >
```

Analogous to this Haskell type:

```haskell
data Package
    = GitHub { repository :: Text, revision :: Text }
    | Local { path :: Text }
    | Hackage { name :: Text, version :: Text }
```

## Pattern matching

```haskell
# ./render.ffg
merge
  { GitHub: \x -> x.repository + "/commit/" + x.revision
  , Local: \x -> x.path
  , Hackage: \x ->
      "https://hackage.haskell.org/package/" + x.name + "-" + x.version
  }
```

Analogous to this `LambdaCase` pattern match:

```haskell
\case
  GitHub{ repository, revision } ->
    repository <> "/commit/" <> revision
  Local{ path } -> path
  Hackage{ name, version } ->
    "https://hackage.haskell.org/package/" + name + "-" + version
```

## Pattern matching

```haskell
>>> List/map ./render.ffg ./packages.ffg
```
```json
[ "https://github.com/Gabriel439/Haskell-Turtle-Library/commit/ae5edf227b515b34c1cb6c89d9c58ea0eece12d5"
, "~/proj/optparse-applicative"
, "~/proj/discrimination"
, "https://hackage.haskell.org/package/lens-4.15.4"
, "https://github.com/haskell/text/commit/ccbfabedea1cf5b38ff19f37549feaf01225e537"
, "~/proj/servant-swagger"
, "https://hackage.haskell.org/package/aeson-1.2.3.0"
]
```

## Polymorphic variants

You only need to handle constructors that are present:

```haskell
>>> List/map (merge { Just: \x -> x }) [ Just 1, Nothing { } ]
Union type mismatch

…
```

```haskell
>>> List/map (merge { Just: \x -> x }) [ Just 1 ]
[ 1 ]
```

```haskell
>>> :type [ Just 1, Nothing { } ]
forall (a : Alternatives) . List < Just: Natural | Nothing: { } | a >
>>> :type [ Just 1 ]
forall (a : Alternatives) . List < Just: Natural | a >
```

## Strong normalization

Grace is strongly normalizing (inspired by Dhall):

```haskell
>>> :let generate = https://raw.githubusercontent.com/Gabriella439/grace/main/prelude/list/generate.ffg

>>> :type generate
forall (a : Type) . Natural -> (Natural -> a) -> List a

>>> generate 10 Integer/even
[ true, false, true, false, true, false, true, false, true, false ]

>>> generate
\n ->
\f ->
  List/map
    (\x -> f x.index)
    (List/indexed (Natural/fold n (\xs -> [ { } ] + xs) [ ]))

>>> generate 10
\f -> [ f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8, f 9 ]
```

# Outline

- Introduction to Fall-from-Grace
- <span style="color:#ff2c2d">Language design</span>
- The Grace Browser

## Similar languages

Grace is heavily inspired by [Dhall](https://dhall-lang.org/)

Grace and Dhall are also both heavily inspired by [Nix](https://nixos.org/)

Grace fixes a lot of the major complaints about Dhall:

- JSON-compatibility
- Better type inference

## Design philosophy

The "zen of Grace" is:

> What if JSON and programming language theory had a baby?

## JSON compatibility

Grace is JSON-compatible in two senses:

- Grace syntax is a superset of JSON

- Grace's type system can handle JSON weirdness

The latter part comprises most of the complexity

## Bidirectional type-checking

Grace uses a bidirectional type-checker

To summarize: "weird shit OK with a type annotation"

Example:

```haskell
[ { x: 1 }, { x: 2, y: true } ]  # Rejected

[ { x: 1 }, { x: 2, y: true } ]  # Accepted
    : List (exists (a : Fields) . { x: Natural, a })
```

## Type safety

```haskell
>>> [ 1, true ]
Not a subtype

The following type:

  Bool

(input):1:6: 
  │
1 │ [ 1, true ]
  │      ↑

… cannot be a subtype of:

  Natural

(input):1:3: 
  │
1 │ [ 1, true ]
  │   ↑
```

## Type safety

```haskell
>>> [ 1, true ] : List JSON
[ 1, true ]
```

```haskell
>>> List/length ([ 1, true ] : List JSON)
2
```

```haskell
>>> List/map (\x -> x + 1) [ 1, true ]
Not a subtype

The following type:

  Bool

(input):1:29: 
  │
1 │ List/map (\x -> x + 1) [ 1, true ]
  │                             ↑

… cannot be a subtype of:

  Natural

(input):1:1: 
  │
1 │ List/map (\x -> x + 1) [ 1, true ]
  │ ↑
```

## Type safety

```haskell
# ./increment.ffg
JSON/fold
  { "bool": \x -> x
  , "natural": \x -> x + 1 : Real
  , "integer": \x -> x + 1 : Real
  , "real": \x -> x + 1 : Real
  , "string": \s -> s
  , "null": null
  , "object": \x -> x
  , "array": \x -> x : JSON
  }
```

```haskell
>>> List/map ./increment.ffg [ 1, true ]
[ 2, true ]

>>> :type List/map ./increment.ffg [ 1, true ]
List JSON
```

## `null` / `Optional`

`null` is the grossest part of JSON-compatibility

```haskell
─────────────────────────────────────
null : forall (a : Type) . Optional a


e : a
──────────────
e : Optional a
```

```haskell
>>> [ 1, null ] : List (Optional Natural)
[ 1, null ]
```

If you don't like it, use `Just` / `Nothing` instead

## `Nothing`, `Just`

You can also use anonymous sums instead:

```haskell
>>> :type [ Just 1, Nothing { } ]
forall (a : Alternatives) . List < Just: Natural | Nothing: { } | a >
```

Or use whatever constructor names you want:

```haskell
>>> :type [ Present 1, Absent { } ]
forall (a : Alternatives) . List < Present: Natural | Absent: { } | a >
```

They're cheap to rename!  No need for a datatype definition.

## No type classes

Grace doesn't support Haskell-style type classes

Coherence incompatible Dhall/Nix/Grace-style imports

The best you can do is [type classes as dictionaries](https://www.haskellforall.com/2012/05/scrap-your-type-classes.html)

## No language support for recursion

Grace does not support any form of recursion

JSON in the wild often uses silent, anonymous recursion:

```json
{ "children": [ { "children": [ 1, 2 ] }, 3 ] }
```

No easy way to type-check that in general

💡 Solution: support only one recursive type: `JSON`

## No user-defined types

Specifically:

- No datatype definitions
- No `newtypes`
- No `type` synonyms

This means:

- No type abstraction
- Worse error messages

This is downside of all types being "anonymous"

## Limited builtin functionality

Grace has a limited set of builtin functions / operators

(Just like Dhall)

The intention is you fork Grace to add what you need

# Outline

- Introduction to Fall-from-Grace
- Language design
- <span style="color:#ff2c2d">The Grace Browser</span>

## "Content-oriented" languages

Grace is a "content-oriented" language, defined as:

> A content-oriented language is a purely functional programming language
> without `IO`

The goal is to [separate content and presentation](https://en.wikipedia.org/wiki/Separation_of_content_and_presentation)

## Example content-oriented languages

- **Calculators**

  Calculations don't include logic for how to read or write values

- **JSON configuration file**

  A separate program uses the configuration file

- **Nix**

  A Nix derivation is just a tree of records

  Nix expressions don't "do" anything[^1]

[^1]: There are some caveats here I won't go into

## Grace Browser

If Grace is the "content" then where is the "presentation"?

One example "presentation" is the Grace Browser:

[https://trygrace.dev](https://trygrace.dev)

The Grace browser separates "content" from "presentation":

- The "content" is the input Grace expression

- The "presentation" is the generated HTML / web form

The input Grace expression has no presentation logic

## Motivation behind separation

Why do we want to [separate content and presentation](https://en.wikipedia.org/wiki/Separation_of_content_and_presentation)?

- **Simplify the user experience**

  Users don't have to care about "presentation details"

- **Support multiple "front-ends"**

  You can present the same content in multiple ways

- **Improve security**

  We can sandbox content devoid of presentation logic

## No `IO`

Grace doesn't support `IO`[^2]

This is because Grace is content-oriented

Most content-oriented languages work this way:

- Content - `IO`-free logic
- Presentation - `IO`-heavy logic

[^2]: … except for language support for imports

## The future of programming

I believe high-level languages will eventually eliminate `IO`

Similar in spirit to how:

- Garbage-collected languages hide memory management details

- Lazy languages hide evaluation order details

See also: [The end of history for programming](https://www.haskellforall.com/2021/04/the-end-of-history-for-programming.html)

# Conclusion

Fork Grace: [https://github.com/Gabriella439/grace](https://github.com/Gabriella439/grace)

Grace Browser: [https://trygrace.dev](https://trygrace.dev)

```haskell
nix run github:Gabriella439/grace -- --help
```

Blog: [https://www.haskellforall.com](https://www.haskellforall.com)

Cohost <img style='vertical-align: bottom' src='https://cohost.org/static/41454e429d62b5cb7963.png' height='55px'/>: [https://cohost.org/Gabriella439](https://cohost.org/Gabriella439)
