% Fall-from-Grace: A Typed and Programmable Superset of JSON
% Gabriella Gonzalez
% December 9, 2022

# Background

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

This "founder effect" primed Haskell to be well-suited for implemented
programming languages

## The missing pieces 🧩

There are three things missing for Haskell to go viral:

- Clean reference implementations of programming languages

  ⭐️ That is this talk ⭐️

- Corresponding books to go with these reference implementations

- Functional logic programming DSL for type-checking

# Outline

- <span style="color:#ff2c2d">Introduction to Fall-from-Grace</span>
- Language design
- Implementation
- The Grace Browser

## Fall-from-Grace

Fall-from-Grace is an interpreted and functional programming language

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

I'll call the language "Grace" as a shorthand throughout this talk

## Build your own language

Grace's primary goal is to be an easy-to-fork language

Easy-to-fork means:

- High quality implementation
- Clear code (as best as I can manage)
- Clear instructions for extending the language

Some people have also expressed interest in using Grace directly

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
# This is a valid Grace program
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
{ clients: List
             { isActive: Bool
             , age: Natural
             , name: Text
             , email: Text
             , phone: Text
             }
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

## Anonymous records

```haskell
```

# TODO

- Talk about why Grace is designed to be forked instead of customized via an API
- Talk about Fall-from-Grace's import system
- Maybe show a live demo of adding subtraction to the language
- Talk about how design of Grace was influenced by lessons learned from Dhall
- Talk about how type-checking is hardest part to implement/customize
- What is Grace "for"?
