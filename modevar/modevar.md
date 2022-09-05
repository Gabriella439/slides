% How to build a sturdy little language
% Gabriella Gonzalez
% October 20, 2020

# Overview

This talk focuses on building an industrial strength language, using the Dhall
configuration language as a model example.

My understanding is that MODEVAR attendees are interested in designing a
standard language for their own purposes.

I hope this talk gives you all ideas for how to do so, including:

* A tour of sample Dhall language features
* How to create a language standard
* How to build polished tooling

My intention is not to get you to use Dhall, but rather to learn from Dhall

# About me

<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@4/distr/fira_code.css">
<style>
code {
  font-family: "Fira Code";
}
</style>

I'm an engineering manager and an open source developer.

My interests are primarily in DevOps, PLT, and LangSec.

I know very little about modeling variability.

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

# Questions?

* A tour of sample Dhall language features
* **How to create a language standard**
* How to build polished tooling

# How to standardize a language

Dhall's most pronounced design constraint supporting multiple language bindings

This constraint entails keeping the language standard as simple as possible

The key components of the standard are:

* The [grammar](https://github.com/dhall-lang/dhall-lang/blob/master/standard/dhall.abnf)

  … which is specified as an ABNF grammar

* The [semantics](https://github.com/dhall-lang/dhall-lang/blob/master/standard/README.md)

  … which are specified using natural deduction

* The [test suite](https://github.com/dhall-lang/dhall-lang/tree/master/tests)

  … which is specified in terms of golden tests

# The standard grammar

Some language bindings generate parsing code directly from the ABNF grammar

Other language bindings hand-translate the grammar rules to parsing code

The grammar is heavily commented to help people who hand-write their parsers:

```abnf
; A simple label cannot be one of the reserved keywords
; listed in the `keyword` rule.
; A PEG parser could use negative lookahead to
; enforce this, e.g. as follows:
; simple-label =
;       keyword 1*simple-label-next-char
;     / !keyword (simple-label-first-char *simple-label-next-char)

simple-label-first-char = ALPHA / "_"

simple-label-next-char = ALPHANUM / "-" / "/" / "_"

simple-label = simple-label-first-char *simple-label-next-char
```

# The standard semantics

Dhall uses natural deduction as a language-independent pseudocode

This is how Dhall standardizes type-checking and evaluation logic:

```haskell
l ⇥ False
──────────────
l && r ⇥ False


r ⇥ False
──────────────
l && r ⇥ False


l ⇥ True   r₀ ⇥ r₁
──────────────────
l && r₀ ⇥ r₁


r ⇥ True   l₀ ⇥ l₁
──────────────────
l₀ && r ⇥ l₁
```

# The standard test suite

The test suite was probably the biggest driver of new language bindings.

The test suite has a subdirectory for each language feature:

```
tests
├── README.md
├── alpha-normalization
│   └── success
├── binary-decode
│   ├── failure
│   └── success
├── import
│   ├── cache
│   ├── data
│   ├── failure
│   └── success
├── normalization
│   └── success
├── parser
│   ├── failure
│   └── success
├── semantic-hash
│   └── success
└── type-inference
    ├── failure
    └── success
```

# Things we could have done differently

We're not entirely sure that our current approach is the best approach

* One alternative is focusing our efforts on a few key language bindings

  For example, we have Rust bindings to Dhall, from which we can derive
  other bindings via C or Webassembly.

  Similarly, we have Java bindings to Dhall, from which we can derive
  Clojure and Scala bindings.

* Another idea is replacing natural deduction pseudocode with simple Haskell
  code

  This would likely be easier for new binding authors to read and understand

  ```haskell
  evaluate (And True  r    ) = evaluate r
  evaluate (And l     True ) = evaluate l
  evaluate (And False _    ) = False
  evaluate (And _     False) = False
  ```

# Standard evolution

Our standard governance process is **very** explicit about voting rights:

* Each independent (non-derived) language binding gets one vote

* Changes are accepted within 3 days if they have majority of approving votes 

* Changes are also accepted within 7 days if they don't have a majority of
  rejections!

* Ties favor accepting the proposal

Despite the formality, we almost never have split votes.  In practice, we
informally reach consensus out-of-band.

Even so, the rules help because they avoid the
[tyranny of structurelessness](https://en.wikipedia.org/wiki/The_Tyranny_of_Structurelessness)

The rules also help convey that the Dhall community is a stable foundation

# Permissions and privileges

More generally, we try to be explicit about how to obtain privileges, including:

* How to get the "commit bit"

  We hand out the commit bit liberally, and publicize that fact

* How to make changes to shared infrastructure

  We use [continuous deployment](https://en.wikipedia.org/wiki/Continuous_deployment),
  meaning that all changes merged to the `master` branch of our repository
  are automatically deployed to shared infrastructure

* How to obtain privileged accounts or credentials

Explicitness assures newcomers that they have a clearly-defined path to greater
autonomy and authority.

# Questions?

* A tour of sample Dhall language features
* How to create a language standard
* **How to build polished tooling**

# Tooling

Dhall has unusually good tooling for a language of its size, including:

* A language server

* A standard formatter

* A REPL

* Integrations with a wide variety of file formats

* A package ecosystem

# Language server

If you only build one tool, build a language server

The [language server protocol](https://langserver.org/) works<sup>\*</sup> with
all IDEs and editors

![](./language-server.gif)

<sup>\*</sup> A tiny bit of work needs to be done for each editor, but it's not hard

# `dhall format`

The language supports a standard code formatter:

```haskell
-- Example code that has been formatted

let JSON = https://prelude.dhall-lang.org/v11.1.0/JSON/package.dhall

let Zone = < us-east-1 | us-west-1 >

let InstanceType = < `m5.large` | `m5.xlarge` >

let Instance = { type : InstanceType, zone : Zone }

let Zone/toJSON =
      λ(zone : Zone) →
        merge
          { us-east-1 = JSON.string "us-east-1"
          , us-west-1 = JSON.string "us-west-1"
          }
          zone

let InstanceType/toJSON =
      λ(type : InstanceType) →
        merge
          { `m5.large` = JSON.string "m5.large"
          , `m5.xlarge` = JSON.string "m5.xlarge"
          }
          type

let Instance/toJSON =
      λ(instance : Instance) →
        JSON.object
          ( toMap
              { type = InstanceType/toJSON instance.type
              , zone = Zone/toJSON instance.zone
              }
          )

let test =
      let example = { type = InstanceType.`m5.xlarge`, zone = Zone.us-east-1 }

      in    assert
          :   JSON.render (Instance/toJSON example)
            ≡ "{ \"type\": \"m5.xlarge\", \"zone\": \"us-east-1\" }"

in  Instance/toJSON
```

# The formatter is fundamental

Mainstream adopters are very passionate about the quality of the auto-formatter

Tools that non-interactively lint/edit/rewrite code usually need to auto-format

Also, error messages need to display formatted expressions:

```dhall
Error: Missing record field: zone2

…

You tried to access a field named:

↳ zone2

... but the field is missing because the record only defines the following
fields:

↳ { type : < `m5.large` | `m5.small` | `m5.xlarge` >
  , zone : < us-east-1 | us-west-1 >
  }

────────────────────────────────────────────────────────────────────────────────

31│                                    instance.zone2

```

# `dhall repl`

A REPL is important for creating tutorial documentation

REPLs also help users interactively explore data and APIs

```haskell
⊢ :let Prelude = https://prelude.dhall-lang.org/package.dhall

⊢ Prelude.<TAB>
Prelude.Bool      Prelude.JSON      Prelude.Monoid    Prelude.XML
Prelude.Double    Prelude.List      Prelude.Natural
Prelude.Function  Prelude.Location  Prelude.Optional
Prelude.Integer   Prelude.Map       Prelude.Text
```

The REPL experience has the potential to be better "documentation" than API docs

# `dhall diff`

One feature unique to Dhall is the `diff` subcommand

This command lets you "diff" two arbitrary Dhall expressions:

```haskell
$ dhall diff \
    'https://prelude.dhall-lang.org/v9.0.0/package.dhall' \
    'https://prelude.dhall-lang.org/v10.0.0/package.dhall'
{ Natural = { + equal = …
            , + greaterThan = …
            , + greaterThanEqual = …
            , + lessThan = …
            , + lessThanEqual = …
            , …
            }
, Text = { + default = …
         , + defaultMap = …
         , …
         }
, …
}
```

# Type errors

Type errors also use the same diff syntax to highlight type mismatches:

```haskell
$ dhall <<< '{ x = 1 } : { y : Natural }'

Use "dhall --explain" for detailed errors

Error: Expression doesn't match annotation

{ - y : …
, + x : …
}

1│ { x = 1 } : { y : Natural }

(input):1:1
```

# `dhall hash`

You can also hash a Dhall expression, to detect if it changed or not:

![](./hash.gif)

This comes in handy when refactoring code, to ensure that refactors are safe

# Things that help tooling quality

The main factors that contribute to quality are:

* The language is small

  … at least compared to a typical mainstream programming language

* The language is reasonably stable

  … due to the standard evolution process

* Almost all of the tooling resides within a monorepo!

# Monorepo

I maintain the Haskell implementation of Dhall, which was the original one

That's why almost all of the Dhall tooling is implemented in Haskell

Early on, I consolidated the implementation and the tooling all in one repo

This was the best decision I ever made!

# Monorepo advantages

Here is why the implementation and tooling should share the same repository:

* The implementation and the tooling should share the same CI

  This ensures that the tooling never lags behind the implementation

  CI rejects PRs if we forget to update the tooling to match the implementation

* The tooling can share the same ops infrastructure

  That in turn helps us release more frequently

* The tooling can share the same development infrastructure

  We also get higher leverage out of any improvements to streamline development

# Conclusion

You can find these slides at:

* [https://github.com/Gabriella439/slides/blob/master/modevar/modevar.md](https://github.com/Gabriella439/slides/blob/master/modevar/modevar.md)

You can learn more about Dhall at:

* [https://dhall-lang.org/](https://dhall-lang.org/)

You can follow my work on GitHub if it interests you:

* [https://github.com/Gabriella439/](https://github.com/Gabriella439/)

# Appendix: Marketing

I'm guessing that some of you have the following questions:

* Will the modelling variability community embrace a language originating in
  MODEVAR?

  Answer: Likely, especially if this conference is central to the community

* Should we actively promote interest in such a language outside our community?

  Answer: Probably not, if the language is specialized to modeling variability

* Is commercial backing or an open source approach better for building a
  language?

  Answer: I recommend open source and discourage commercializing programming
  language endeavours.  The needs of commercial interests are generally at odds
  with the needs of a language standard.

* Will quality suffer if our language does not have enough users and is not
  proprietary?

  Answer: Not if the language's scope is small and well-defined.  You don't need
  a lot of contributors to maintain a tiny interpreted language.

Also, people interested in marketing should watch this other talk I've given:

* [How to market Haskell to a mainstream programmer](https://www.youtube.com/watch?v=fNpsgTIpODA) ([Slides](https://github.com/Gabriella439/slides/blob/master/marketing/marketing.md))


