% How to create a new Haskell project
% Gabriella Gonzalez
% April 8, 2017

# Goal

This talk will cover:

* How to bootstrap a package for private use
* How to set up an open source project on Github
* How to publish your package to Hackage and Stackage

... and assorted useful tips for maintaining packages

# Download `stack`

Go to [haskellstack.org](http://haskellstack.org)

The landing page tells you how to download `stack`:

```bash
$ curl -sSL https://get.haskellstack.org/ | sh
```

# Create a new project - Manual

```haskell
-- Main.hs
main :: IO ()
main = putStrLn "Hello, world!"
```

```haskell
-- example.cabal
name: example
version: 1.0.0
build-type: Simple

executable hello
    build-depends: base < 5
    main-is: Main.hs
```

```haskell
-- Setup.hs
import Distribution.Simple
main = defaultMain
```

# `cabal-install` vs `stack`

If you prefer to use `stack`, then add this file to your project:

```yaml
# stack.yaml
resolver: lts-8.6
```

If you prefer to use `cabal-install`, then do this:

```bash
wget https://www.stackage.org/lts-8.6/cabal.config
```

This instructs `cabal` to pick the same package versions that `stack` would

# `stack` 

This talk will focus on `stack`-based workflows, mainly because:

* `stack` is much easier for projects spanning multiple repositories
* `stack` manages and isolates the Haskell compiler for you
* `stack` command line API is nicer and more featureful than `cabal`

I use Nix + `cabal-install` for Haskell development, but not beginner-friendly:

* [https://github.com/Gabriella439/haskell-nix](https://github.com/Gabriella439/haskell-nix)

# Build and run the executable

```bash
$ stack setup  # One-time setup if you've never used `stack` before
$ stack build
...
$ stack exec hello
Hello, world!
```

Install the executable in a global path:

```bash
$ stack install
Copying from /private/.../bin/hello to /Users/gabriella/.local/bin/hello

Copied executables to /Users/gabriella/.local/bin:
- hello
```

Consider adding `~/.local/bin` to your `$PATH`

# Create a new project - Generated

```bash
$ stack new example
...
$ cd example
```

Explore the generated project

# Use `stack` templates

`stack` provides several predefined project templates

```bash
$ stack templates
Template                    Description
chrisdone                 
foundation                - Project based on an alternative prelude with batteries and no dependencies.
franklinchen              
ghcjs                     - Haskell to JavaScript compiler, based on GHC
ghcjs-old-base            
hakyll-template           - a static website compiler library
haskeleton                - a project skeleton for Haskell packages
hspec                     - a testing framework for Haskell inspired by the Ruby library RSpec
new-template              
protolude                 - Project using a custom Prelude based on the Protolude library
quickcheck-test-framework - a library for random testing of program properties
readme-lhs                - small scale, quick start, literate haskell projects
rubik                     
scotty-hello-world        
scotty-hspec-wai          
servant                   - a set of packages for declaring web APIs at the type-level
servant-docker            
simple                    
simple-hpack              
simple-library            
spock                     - a lightweight web framework
tasty-discover            - a project with tasty-discover with setup
tasty-travis              
unicode-syntax-exe        
unicode-syntax-lib        
yesod-minimal             
yesod-mongo               
yesod-mysql               
yesod-postgres            
yesod-postgres-fay        
yesod-simple              
yesod-sqlite              
```

# Use `--file-watch`

```bash
$ stack build --file-watch
Warning: File listed in example.cabal file does not exist: README.md
ExitSuccess
Type help for available commands. Press enter to force a rebuild.
```

Now edit `src/Lib.hs` in another window and save the file

# Doctests

You can use the `doctest` library to add tests inline within documentation

The `doctest` library automatically detects and runs these tests

# Useful testing links

Up-front unit testing in Haskell:

* [https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md)

# Versioning

Package versioning policy (PVP):

```
        +-- Major release, breaking changes
        |
        | +-- Minor release, breaking changes
        | |
        | | +-- Minor release, no breaking changes
        | | |
        v v v
Version X.Y.Z
```

This convention differs from other language ecosystems

# Add a `README.md`

Github will render the `README.md` on your project's landing page:

* [https://github.com/ekmett/lens](https://github.com/ekmett/lens)

The default `stack` template will also include your `README.md` on Hackage

Good things to include in your `README.md`:

* Project title and current version
* Brief summary of the project
* Quick start instructions
* Link to documentation
* How to contribute
* License

# Add a `CHANGELOG.md`

Example:

* [https://hackage.haskell.org/package/lens-4.15.1/changelog](https://hackage.haskell.org/package/lens-4.15.1/changelog)

Useful things to include:

* New functionality added
* Breaking changes

Bonus: document the version when each new function was added:

* [https://hackage.haskell.org/package/http-client-0.5.6.1/docs/Network-HTTP-Client.html](https://hackage.haskell.org/package/http-client-0.5.6.1/docs/Network-HTTP-Client.html)

This makes it easier for people to figure out their lower bounds

# CI

I'm only familiar with Travis for open source projects

Shout out to the `multi-ghc-travis` project:

* [https://github.com/hvr/multi-ghc-travis](https://github.com/hvr/multi-ghc-travis)

Let's add this line to our `*.cabal` file:

```
Tested-With: GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.2
```

... then run:

```
path/to/make_travis_yml.hs example.cabal > .travis.yml
```

... then check in the `.travis.yml` file and enable Travis for your repository

# Examples

**ADD AT LEAST ONE END-TO-END EXAMPLE TO YOUR HADDOCKS!!!!!!!!!!!!!!!!!!!!!!!!!!**

If the example is in your `README.md`, included it in the Haddocks

This takes 5 minutes of your time and saves each one of your users much more time

# Documentation checklist

Things that I believe you should do for all projects:

* Browse your project's haddocks
* Haddock coverage close to 100%
* One end-to-end example
* `README.md`

Things that I consider optional:

* Tutorial
* Blog post to announce the library

# Publishing to Hackage

First you need to request a Hackage account (i.e. username/password):

* [https://hackage.haskell.org/users/register-request](https://hackage.haskell.org/users/register-request)

Once you have that, you can run:

```bash
$ stack upload .
```

`stack` will prompt you for your Hackage username and password

`stack` will reuse your credentials for subsequent uploads

# **HELP!**  I don't see the latest version of my package

For example, suppose I upload a new version of `pipes` and I don't see the
uploaded version here:

* [https://hackage.haskell.org/package/pipes](https://hackage.haskell.org/package/pipes)

This is usually because your browser cached the package landing page

You can either invalidate your cache or visit the specific package version at:

* [https://hackage.haskell.org/package/pipes-4.3.3](https://hackage.haskell.org/package/pipes-4.3.3)

# **HELP!** My documentation doesn't render

If your documentation doesn't render after you upload:

*   Wait 1 hour for Hackage to build the documentation for your package

    Example for `turtle`:

    * [https://hackage.haskell.org/package/turtle](https://hackage.haskell.org/package/turtle)
    * [https://hackage.haskell.org/package/turtle-1.3.2/reports/1](https://hackage.haskell.org/package/turtle-1.3.2/reports/1)
   
*   If that still fails, then:

    ```bash
    $ cabal upload --doc
    ```

    ... which will bypass Hackage and explicitly build and upload docs locally

    The downside is that some links to other packages might not exactly work

# Publish to Stackage

You can find instructions on how to add a package to Stackage here:

* [https://github.com/fpco/stackage#add-your-package](https://github.com/fpco/stackage#add-your-package)

The main part is just adding your package to this file:

* [https://github.com/fpco/stackage/blob/master/build-constraints.yaml](https://github.com/fpco/stackage/blob/master/build-constraints.yaml)

Then every once in a while you'll get an e-mail like this:

* [https://github.com/fpco/stackage/issues/2393](https://github.com/fpco/stackage/issues/2393)

# Conclusion

I hope this inspires some people at BayHac to start their own projects!

You can find these slides at

* [https://github.com/Gabriella439/slides/tree/master/bayhac2017](https://github.com/Gabriella439/slides/tree/master/bayhac2017)

If you ever have questions, hit me up at @GabriellaG439 on Twitter
