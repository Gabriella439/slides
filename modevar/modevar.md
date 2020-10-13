% How to build a sturdy language
% Gabriel Gonzalez
% October 20, 2020

# About me

<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@4/distr/fira_code.css">
<style>
code {
  font-family: "Fira Code";
}
</style>

I'm an engineering manager at Arista Networks and an open source developer.

My interests are primarily in DevOps, PLT, and LangSec.

I know very little about modeling variability.

# Overview

This talk focuses on building an industrial strength language, using the Dhall
configuration language as a model example.

My understanding is that MODEVAR attendees are interested in designing a
standard language for their own purposes.

I hope this talk gives you all ideas for how to do so, including:

* Ideas for language features
* How to create a language standard
* How to promote adoption of that language
* How to build polished tooling

# What is Dhall?

Dhall is a [programmable configuration language](https://docs.dhall-lang.org/discussions/Programmable-configuration-files.html)
that you can think of as: **JSON + functions + types + imports**

Dhall originated as an academic exercise to settle the following question:

> Would people tolerate programmable configuration files if they were not
> Turing complete?

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

You can also convert Dhall to JSON (for ease of migration):

```json
{
  "home":       "/home/bill",
  "privateKey": "/home/bill/.ssh/id_ed25519",
  "publicKey":  "/home/bill/.ssh/id_ed25519.pub"
}
```

# Dhall is a programming language

This means that you can factor out shared logic into (immutable) variables:

```haskell
let user = "bill"

in  { home       = "/home/${user}"
    , privateKey = "/home/${user}/.ssh/id_ed25519"
    , publicKey  = "/home/${user}/.ssh/id_ed25519.pub"
    }
```

… and use programming features like string interpolation.

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

`x : T` means that `x` has type `T`

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
let generate = https://prelude.dhall-lang.org/List/generate

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

A lot of design went into making URL imports secure

# Emergent properties

You can implement derived features by mixing these simpler features

For example, a package is just a (potentially nested) record that you import:

```haskell
let Prelude = https://prelude.dhall-lang.org

in  Prelude.Bool.not True
```

… and a template is just a Dhall function that interpolates text:

```haskell
\(companyName : Text) ->
\(companySize : Natural) ->
  ''
  <html>
  <title>Welcome to ${companyName}!</title>
  <body>
  <p>Welcome to our humble company of ${Natural/show companySize} people!</p>
  </body>
  </html>
  ''
```

# Conclusion

You can find these slides at: …
