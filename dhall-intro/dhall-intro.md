% Dhall: A programmable configuration language
% Gabriel Gonzalez
% December 15, 2020

# Overview

<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@4/distr/fira_code.css">
<style>
code {
  font-family: "Fira Code";
}
</style>

This talk introduces the Dhall configuration language to functional programmers.

In this talk, I will cover:

* Using Dhall as an ordinary configuration file format
* Using Dhall as a programming language
* Dhall's tooling
* Dhall's language security guarantees

# What is Dhall?

Dhall is a [programmable configuration language](https://docs.dhall-lang.org/discussions/Programmable-configuration-files.html)
that you can think of as: **JSON + functions + types + imports**

Dhall originated as an academic exercise to settle the following question:

> Would people tolerate programmable configuration files if they were not
> Turing-complete?

The project then grew beyond my expectations, so I went with the flow and
learned a lot along the way.

# Dhall is like JSON

When I say Dhall is like JSON I mean that:

* Dhall can model scalars, records, and lists (and other things)
* You can marshal a Dhall configuration file directly into a program as data

Here is an example Dhall configuration file:

```haskell
{ home       = "/home/bill"
, privateKey = "/home/bill/.ssh/id_ed25519"
, publicKey  = "/home/bill/.ssh/id_ed25519.pub"
}
```

… which corresponds to this JSON:

```bash
$ dhall-to-json --file ./config.dhall | tee ./config.json
```
```json
{
  "home":       "/home/bill",
  "privateKey": "/home/bill/.ssh/id_ed25519",
  "publicKey":  "/home/bill/.ssh/id_ed25519.pub"
}
```

# JSON to Dhall

You can migrate an existing JSON file to Dhall.For example, this JSON:

```json
[
  {
    "discontinued": false,
    "description": "Google Pixel XL 128GB Unlocked"
  },
  {
    "description": "Apple iPhone SE (64GB, Black)"
  }
]
```

… corresponds to this Dhall configuration:

```bash
$ json-to-dhall --file ./inventory.json
```
```haskell
[ { description = "Google Pixel XL 128GB Unlocked"
  , discontinued = Some False
  }
, { description = "Apple iPhone SE (64GB, Black)"
  , discontinued = None Bool
  }
]
```

# Dhall can also generate YAML

```haskell
[ { name = "John Doe"
  , age = 14
  }
, { name = "Mary Jane"
  , age = 13
  }
]
```

…  corresponds to this YAML:

```bash
$ dhall-to-yaml --file ./students.dhall 
```
```yaml
- age: 14
  name: John Doe
- age: 13
  name: Mary Jane
```

# YAML to Dhall

You can also convert YAML to Dhall.  For example, this YAML:

```yaml
docker:
    - image: ubuntu:14.04
    - image: mongo:2.6.8
      command: [mongod, --smallfiles]
    - image: postgres:9.4.1
```

… corresponds to this Dhall configuration:

```bash
$ yaml-to-dhall --file ./docker.yaml
```
```haskell
{ docker =
  [ { command = None (List Text)
    , image = "ubuntu:14.04"
    }
  , { command = Some [ "mongod", "--smallfiles" ]
    , image = "mongo:2.6.8"
    }
  , { command = None (List Text)
    , image = "postgres:9.4.1"
    }
  ]
}
```

# Real-world example - Dhall to YAML

Here is a more "real world" example of a Dhall configuration for Kubernetes:

```haskell
let kubernetes = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/a4126b7f8f0c0935e4d86f0f596176c41efbe6fe/package.dhall sha256:ef3845f617b91eaea1b7abb5bd62aeebffd04bcc592d82b7bd6b39dda5e5d545

in  kubernetes.Deployment::{
    , metadata = kubernetes.ObjectMeta::{ name = Some "nginx" }
    , spec = Some kubernetes.DeploymentSpec::{
      , selector = kubernetes.LabelSelector::{
        , matchLabels = Some (toMap { name = "nginx" })
        }
      , template = kubernetes.PodTemplateSpec::{
        , metadata = kubernetes.ObjectMeta::{ name = Some "nginx" }
        , spec = Some kubernetes.PodSpec::{
          , containers =
            [ kubernetes.Container::{
              , name = "nginx"
              , image = Some "nginx:1.15.3"
              , ports = Some
                [ kubernetes.ContainerPort::{ containerPort = 80 } ]
              }
            ]
          }
        }
      }
    }
```

# Real-world example - Dhall to YAML

… which corresponds to this YAML Kubernetes configuration:

```bash
$ dhall-to-yaml --file ./deployment.dhall
```
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nginx
spec:
  selector:
    matchLabels:
      name: nginx
  template:
    metadata:
      name: nginx
    spec:
      containers:
        - image: nginx:1.15.3
          name: nginx
          ports:
            - containerPort: 80
```

# Real world example - YAML to Dhall

The reverse direction works, too, if we provide a bit more information:

```bash
$ yaml-to-dhall '(https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/a4126b7f8f0c0935e4d86f0f596176c41efbe6fe/types.dhall).Deployment' --file ./deployment.yaml \
  | dhall rewrite-with-schemas --schemas 'https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/a4126b7f8f0c0935e4d86f0f596176c41efbe6fe/schemas.dhall'
```
```haskell
let schemas = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/a4126b7f8f0c0935e4d86f0f596176c41efbe6fe/schemas.dhall

in  schemas.Deployment::{
    , metadata = schemas.ObjectMeta::{ name = Some "nginx" }
    , spec = Some schemas.DeploymentSpec::{
      , selector = schemas.LabelSelector::{
        , matchLabels = Some [ { mapKey = "name", mapValue = "nginx" } ]
        }
      , template = schemas.PodTemplateSpec::{
        , metadata = schemas.ObjectMeta::{ name = Some "nginx" }
        , spec = Some schemas.PodSpec::{
          , containers =
            [ schemas.Container::{
              , image = Some "nginx:1.15.3"
              , name = "nginx"
              , ports = Some [ schemas.ContainerPort::{ containerPort = 80 } ]
              }
            ]
          }
        }
      }
    }
```

# Other supported file formats

You can convert between Dhall and the following file formats:

* JSON - `dhall-to-json` / `json-to-dhall`
* YAML - `dhall-to-yaml` / `yaml-to-dhall`
* XML - `dhall-to-xml` / `xml-to-dhall`
* Bash - `dhall-to-bash`
* Nix - `dhall-to-nix`

# Native language bindings

Dhall is not just a preprocessor for JSON/YAML

Some languages (like Haskell) can read Dhall files without going through JSON:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

import Dhall (Generic, FromDhall, Text)

import qualified Dhall

data Configuration = Configuration
    { home       :: Text
    , privateKey :: Text
    , publicKey  :: Text
    } deriving (Generic, FromDhall, Show)

main :: IO ()
main = do
    config <- Dhall.input Dhall.auto "./config.dhall"

    print (config :: Configuration)
```

# Why not go through JSON?

There are some benefits of reading a Dhall configuration file directly:

* Simplicity

* Marshalling unions (a.k.a. sum types)

  JSON/YAML don't natively support sum types

* Marshalling functions

  For example, this works:

  ```haskell
  f :: (Natural -> [Natural]) <- Dhall.input Dhall.auto "λ(x : Natural) → [ x, x ]"

  print (f 2)  -- [ 2, 2 ]
  ```

  This is limited to monomorphic non-higher-order functions

# Language bindings

The currently official language bindings are:

* Clojure
* Haskell
* Go
* Ruby
* Rust

… and there is one unofficial language binding that is worth mentioning:

* Java - Fairly comprehensive

The most wished-for language binding is Python

The latest list can be found at:

* [`docs.dhall-lang.org` - How to integrate Dhall](https://docs.dhall-lang.org/howtos/How-to-integrate-Dhall.html)

# Questions?

# Dhall is a programming language

This means that you can factor out shared logic into (immutable) variables:

```haskell
let user = "bill"

in  { home       = "/home/${user}"
    , privateKey = "/home/${user}/.ssh/id_ed25519"
    , publicKey  = "/home/${user}/.ssh/id_ed25519.pub"
    }
```

… and use programming language features like string interpolation.

# Data model - simple types

Dhall supports the following simple ("scalar") types:

* `Bool`: `True`, `False`
* `Text`: `"Hello, world!"`
* `Natural`: `0`, `1`, `2`, …
* `Integer`: …, `-2`, `-1`, `+0`, `+1`, `+2`, …
* `Double`: `3.14159265`, `6.0221409e+23`

# Data model - complex types

Dhall also supports the following complex ("composite") types:

* `Optional`: `Some 1`, `None Natural`
* `List`: `[ 2, 3, 5 ]`, `[] : List Natural`
* Records: `{ x = 1.2, y = -2.5 }`
* Unions: `let Example = < Left : Natural | Right : Bool > in Example.Left 0`
    * Enums: `let DNA = < A | T | G | C > in [ DNA.A, DNA.T, DNA.C ]`

Complex types can be nested arbitrarily (like JSON/YAML, unlike INI/TOML)

# `Text`

Dhall's `Text` support is best-in-class among configuration languages

For example, Dhall supports string interpolation:

```haskell
λ(name : Text) → "Hello, ${name}!"
```

… and multi-line strings with automatic dedentation:

```haskell
let header = "Header"

let content = "Content"

in  ''
    <!DOCTYPE html>
    <html>
    <body>
    <h1>${header}</h1>
    <p>${content}</p>
    </body>
    </html>
    ''
```

# Records

Dhall supports Nix-style dotted-field syntax.  For example, this:

```haskell
{ a.b.c = 1, a.b.d = True }
```

… is the same thing as:

```haskell
{ a = { b = { c = 1, d = True } } }
```

This comes in handy when working with deeply-nested records.

Dhall is more flexible than Nix.  For example, this works in Dhall:

```haskell
{ a.b.c = 1, a = { b.d = 2 } }
```

… but does not work in Nix.

# Dhall has functions

You can avoid repeating yourself (DRY) using functions:

```haskell
let makeUser = \(user : Text) ->
      let home       = "/home/${user}"
      let privateKey = "${home}/.ssh/id_ed25519"
      let publicKey  = "${privateKey}.pub"
      in  { home, privateKey, publicKey }
in  [ makeUser "bill"
    , makeUser "jane"
    ]
```

… which is equivalent to this JSON:

```json
[
  {
    "home":       "/home/bill",
    "privateKey": "/home/bill/.ssh/id_ed25519",
    "publicKey":  "/home/bill/.ssh/id_ed25519.pub"
  },
  {
    "home":       "/home/jane",
    "privateKey": "/home/jane/.ssh/id_ed25519",
    "publicKey":  "/home/jane/.ssh/id_ed25519.pub"
  }
]
```

# Dhall is typed

You can validate configuration files ahead of time using types:

```haskell
let Config : Type =
      { home       : Text
      , privateKey : Text
      , publicKey  : Text
      }

let makeUser : Text -> Config = \(user : Text) ->
      let home       : Text   = "/home/${user}"
      let privateKey : Text   = "${home}/.ssh/id_ed25519"
      let publicKey  : Text   = "${privateKey}.pub"
      let config     : Config = { home, privateKey, publicKey }
      in  config

let configs : List Config =
      [ makeUser "bill"
      , makeUser "jane"
      ]

in  configs
```

# Dhall has imports

You can embed other expressions by path:

```haskell
-- ./number.dhall

2
```

```haskell
-- This is valid Dhall code
./number.dhall + ./number.dhall
```

```haskell
-- More common idiom, equivalent to the previous code:
let number = ./number.dhall

in  number + number
```

… or by environment variable:

```haskell
"Hello, my name is ${env:USER as Text}"
```

# Dhall has imports

You can also embed other expressions by URL!

```haskell
let generate = https://prelude.dhall-lang.org/List/generate.dhall

let makeUser = \(user : Text) ->
      let home       = "/home/${user}"
      let privateKey = "${home}/.ssh/id_ed25519"
      let publicKey  = "${privateKey}.pub"
      in  { home, privateKey, publicKey }

let buildUser = \(index : Natural) ->
      makeUser "build${Natural/show index}"

let Config =
      { home : Text
      , privateKey : Text
      , publicKey : Text
      }

in  generate 10 Config buildUser
```

The language design ensures that URL imports are secure

# Dhall has tests

Dhall provides language support for tests!

```haskell
--| `lessThanEqual` checks if one Natural is less than or equal to another.
let lessThanEqual
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → Natural/isZero (Natural/subtract y x)

let example0 = assert : lessThanEqual 5 6 ≡ True

let example1 = assert : lessThanEqual 5 5 ≡ True

let example2 = assert : lessThanEqual 5 4 ≡ False

…

in  lessThanEqual
```

# Emergent properties - package

You can implement derived features by mixing these simpler features

For example, a package is just a (potentially nested) record that you import:

```haskell
let Prelude = https://prelude.dhall-lang.org/v20.0.0/package.dhall

in  Prelude.Bool.not True
```

# Emergent properties - template

In Dhall, a template is just a function that interpolates `Text`:

```haskell
\(args : { year : Natural, copyrightHolder : Text }) ->
  ''
  Copyright ${Natural/show args.year} ${args.copyrightHolder}

  Permission is hereby granted, free of charge, to any person obtaining a copy of 
  this software and associated documentation files (the "Software"), to deal in 
  the Software without restriction, including without limitation the rights to 
  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
  of the Software, and to permit persons to whom the Software is furnished to do 
  so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all 
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
  SOFTWARE.
  ''
```

# Emergent properties - template

Here is an example of how you would apply a template using `dhall text`:

```bash
$ dhall text <<< './template.dhall { year = 2020, copyrightHolder = "Gabriel Gonzalez" }'
Copyright 2020 Gabriel Gonzalez

Permission is hereby granted, free of charge, to any person obtaining a copy of 
this software and associated documentation files (the "Software"), to deal in 
the Software without restriction, including without limitation the rights to 
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
of the Software, and to permit persons to whom the Software is furnished to do 
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all 
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
SOFTWARE.
```

# Emergent properties - schema

A schema is just a type annotation that you import:

```haskell
-- ./schema.dhall
{ home : Text, privateKey : Text, publicKey : Text }
```

```haskell
-- ./config.dhall
{ home       = "/home/bill"
, privateKey = "/home/bill/.ssh/id_ed25519"
, publicKey  = "/home/bill/.ssh/id_ed25519.pub"
} : ./schema.dhall
```

# Emergent properties - Property tests

A property test is just a unit test where both sides have the same normal form:

```haskell
--| `lessThanEqual` checks if one Natural is less than or equal to another.
let lessThanEqual
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → Natural/isZero (Natural/subtract y x)

…

let property0 = λ(n : Natural) → assert : lessThanEqual 0 n ≡ True

let property1 = λ(n : Natural) → assert : lessThanEqual n n ≡ True

in  lessThanEqual
```

This works because the interpreter can normalize abstract expressions

# Dhall is total

Dhall is a *total* functional programming language

This means Dhall configuration files never hang, crash, or throw exceptions

One benefit of being total is strong normalization:

```haskell
$ dhall repl
⊢ :let generate = https://prelude.dhall-lang.org/List/generate.dhall

generate : ∀(n : Natural) → ∀(a : Type) → ∀(f : Natural → a) → List a

⊢ generate 10

λ(a : Type) →
λ(f : Natural → a) →
  [ f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8, f 9 ]
```

# Lack of recursion

Dhall does not provide language support for recursion

Recursion is not impossible; it's just not ergonomic.

See: [`docs.dhall-lang.org` - How to translate recursive code to Dhall](https://docs.dhall-lang.org/howtos/How-to-translate-recursive-code-to-Dhall.html)

# Dhall's type inference is limited

Dhall does not (yet) support bidirectional type inference

This means that the language requires types or type annotations in a few places,
like:

* An empty list:

  ```haskell
  [] : List Natural
  ```

* An empty `Optional` value:

  ```haskell
  None Natural
  ```

* A function's input type:

  ```haskell
  \(x : Natural) -> x + 1
  ```

* Specializing a polymorphic function:

  ```haskell
  Prelude.List.map Natural Natural (\(x : Natural) -> x + 1)
  ```

# Questions?

# Prelude

The most widely used Dhall package is the Prelude:

```haskell
let Prelude = https://prelude.dhall-lang.org/v20.0.0/package.dhall
```

# REPL

```haskell
$ dhall repl
⊢ :let Prelude = https://prelude.dhall-lang.org/package.dhall
…

⊢ Prelude.<TAB>
Prelude.Bool      Prelude.JSON      Prelude.Monoid    Prelude.Text
Prelude.Double    Prelude.List      Prelude.Natural   Prelude.XML
Prelude.Function  Prelude.Location  Prelude.Operator
Prelude.Integer   Prelude.Map       Prelude.Optional

⊢ Prelude.Natural.<TAB>
Prelude.Natural.build             Prelude.Natural.listMin
Prelude.Natural.enumerate         Prelude.Natural.max
Prelude.Natural.equal             Prelude.Natural.min
Prelude.Natural.even              Prelude.Natural.odd
Prelude.Natural.fold              Prelude.Natural.product
Prelude.Natural.greaterThan       Prelude.Natural.show
Prelude.Natural.greaterThanEqual  Prelude.Natural.sort
Prelude.Natural.isZero            Prelude.Natural.subtract
Prelude.Natural.lessThan          Prelude.Natural.sum
Prelude.Natural.lessThanEqual     Prelude.Natural.toDouble
Prelude.Natural.listMax           Prelude.Natural.toInteger

⊢ Prelude.Natural.product [ 2, 3, 5 ]

30
```

# Rich diff - command line

The interpreter supports rich "diff"s of arbitrary expressions.  For example:

```haskell
$ dhall diff '{ x = [ 1, 2, 3 ], y = True }' '{ x = [ 2, 3, 4 ], z = "ABC" }'
{ - y = …
, + z = …
,   x = [ - 1
        , …
        , + 4
        ]

}
```

Only the difference is displayed.  Fields/elements/values in common are omitted

This feature is used in two places to improve the user experience

# Rich diff - type errors

The interpreter uses rich diffs to highlight what went wrong for type errors:

```haskell
⊢ :let Person = { name : Text, age : Natural }

Person : Type

⊢ { name = "John Doe", agee = 24 } : Person

Error: Expression doesn't match annotation

{ - age : …
, + agee : …
, …
}

1│ { name = "John Doe", agee = 24 } : Person

(input):1:1
```

This makes it easy to drill down to the offending typo, even for large records.

# Rich diff - tests

Also, test failures will display a rich diff:

```haskell
Error: Assertion failed

"[▮true, 1▮]"

1│ assert : JSON.renderCompact (JSON.array [ JSON.bool True, JSON.natural 1 ]) === "[true, 1]"

(input):1:1
```

# Hashing expressions - refactors

The language also supports hashing arbitrary expressions

Dhall hashes normal forms, so two αβ-equivalent expressions have the same hash:

```bash
$ dhall hash <<< '\(x : Bool) -> [ x && True, x || False ]'
sha256:486b561c6e38adf2c2853b6395358c16c3ed1befc35c33067996dcfb51a74e62

$ dhall hash <<< '\(y : Bool) -> [ y, y ]'
sha256:486b561c6e38adf2c2853b6395358c16c3ed1befc35c33067996dcfb51a74e62
```

You can verify that a refactor is behavior-preserving using these hashes

If the hash doesn't change, then it is a behavior-preserving refactor

These hashes are insensitive to:

* Comments / coding style
* Variable names
* Module organization

# Hashing expressions - integrity checks

You can protect a remote import with a hash of the import, like this:

```haskell
let Prelude =
      https://prelude.dhall-lang.org/v20.0.0/package.dhall
        sha256:21754b84b493b98682e73f64d9d57b18e1ca36a118b81b33d0a243de8455814b

…
```

The hash serves two purposes here:

* The import is always verified against the hash to protect against tampering

* The import is cached locally in a content-addressable store

  … where the hash is the lookup key

# Dhall tooling - TODO

* Prelude
* REPL
* Hashing / integrity checks
* `dhall-docs`
* `dhall diff` / type-level diffs
* `dhall hash`
* `dhall-lsp-server`
* Emergent properties (rich diff + imports = change log)
* No equivalent of Hackage, yet

# TODO

Things to browse for talk material:

* Language tour
* Twitter account
