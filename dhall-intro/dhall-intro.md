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

This talk is an introduction to Dhall for functional programmers.

In this talk, I will cover:

* How to use Dhall as a programmable configuration file format
* How Dhall compares to other alternatives
* How to harness Dhall's language security guarantees

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

You can also migrate an existing JSON configuration to Dhall

For example, this JSON:

```json
[
  { "id": 0,
    "discontinued": false,
    "description": "Google Pixel XL 128GB Unlocked"
  },
  {
    "id": 1,
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
  , id = 0
  }
, { description = "Apple iPhone SE (64GB, Black)"
  , discontinued = None Bool
  , id = 1
  }
]
```

# Dhall can also generate YAML

```haskell
-- ./students.dhall

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

You can also convert YAML to Dhall

For example, this YAML:

```yaml
execution:
- concurrency: 10
  hold-for: 5m
  ramp-up: 2m
  scenario: yaml_example
  
scenarios:
  yaml_example:
    retrieve-resources: false
    requests:
      - http://example.com/
```

… corresponds to this Dhall configuration:

```bash
$ yaml-to-dhall --file ./ci.yaml
```
```haskell
{ execution =
  [ { concurrency = 10
    , hold-for = "5m"
    , ramp-up = "2m"
    , scenario = "yaml_example"
    }
  ]
, scenarios.yaml_example =
  { requests = [ "http://example.com/" ]
  , retrieve-resources = False
  }
}
```

# Other supported file formats

You can convert between Dhall and the following file formats:

* JSON - `dhall-to-json` / `json-to-dhall`
* YAML - `dhall-to-yaml` / `yaml-to-dhall`
* XML - `dhall-to-xml` / `xml-to-dhall`
* Bash - `dhall-to-bash`

## Real-world example - Dhall to YAML

Here is a more "real world" example of a Dhall configuration for Kubernetes:

```haskell
-- ./deployment.dhall

let kubernetes =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/package.dhall sha256:ef3845f617b91eaea1b7abb5bd62aeebffd04bcc592d82b7bd6b39dda5e5d545

in  kubernetes.Deployment::{
    , metadata = kubernetes.ObjectMeta::{ name = Some "nginx" }
    , spec = Some kubernetes.DeploymentSpec::{
      , selector = kubernetes.LabelSelector::{
        , matchLabels = Some (toMap { name = "nginx" })
        }
      , replicas = Some 2
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
  replicas: 2
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
$ yaml-to-dhall '(https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/a4126b7f8f0c0935e4d86f0f596176c41efbe6fe/types.dhall).Deployment' --file ./kubernetes.yaml \
  | dhall rewrite-with-schemas --schemas 'https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/a4126b7f8f0c0935e4d86f0f596176c41efbe6fe/schemas.dhall'
```
```haskell
let schemas =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/a4126b7f8f0c0935e4d86f0f596176c41efbe6fe/schemas.dhall

in  schemas.Deployment::{
    , metadata = schemas.ObjectMeta::{ name = Some "nginx" }
    , spec = Some schemas.DeploymentSpec::{
      , replicas = Some 2
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

# Native language bindings

You can read a Dhall configuration file directly without going through a JSON
intermediate, given a language binding:

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

# Language bindings

The currently available language bindings are:

* Clojure
* Haskell
* Java (Unofficial)
* Go
* Ruby
* Rust

The most wished-for binding: Python

The latest list can be found at:

* [`docs.dhall-lang.org` - How to integrate Dhall](https://docs.dhall-lang.org/howtos/How-to-integrate-Dhall.html)

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

# Emergent properties

You can implement derived features by mixing these simpler features

For example, a package is just a (potentially nested) record that you import:

```haskell
let Prelude = https://prelude.dhall-lang.org/package.dhall

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