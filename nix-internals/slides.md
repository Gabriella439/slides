% Nix: under the hood
% Gabriel Gonzalez
% December 7, 2017

# Motivation

This talk covers how to **use** and **debug** Nix effectively in a commercial
setting

We'll do so by learning how Nix works under the hood

This talk will focus more on breadth rather than depth

This talk will emphasize the Nix toolchain and de-emphasize the Nix language 

Key ideas:

* The `/nix/store` and NixOS don't depend on the Nix language
* Nix supports binary distributions (both for packages and operating systems)
* Don't save secrets in Nix derivations
* Caching happens at the derivation level
* Source vs. binary distributions
* Profiles
* TODO

# Introduction

Most Nix workflows go through this pipeline:

```
Nix expression  →  Derivation  →  Build product
```

We'll organize this talk around those three stages in the pipeline

# Organization

I like to document my Haskell libraries using the following standard format:

* For each type in the API:
    * Document what each type represents
    * Document how to **produce** a value of that type
    * Document how to **transform** a value of that type
    * Document how to **consume** a value of that type

We'll use a similar approach to systematically explore the Nix pipeline

# Overview

* Nix expressions
    * What are Nix expressions?
    * How do we **produce** Nix expressions?
    * How do we **transform** Nix expressions?
    * How do we **consume** Nix expressions?
* Derivations
    * What is a derivation?
    * How do we **produce** a derivation?
    * How do we **transform** a derivation?
    * How do we **consume** a derivation?
* Build products
    * What is a build product?
    * How do we **produce** a build product?
    * How do we **transform** a build product?
    * How do we **consume** a build product?

# Overview

* Source code
    * What are Nix expressions?
    * How do we produce Nix expressions?
    * How do we transform Nix expressions?
    * How do we consume Nix expressions?
* Derivations
    * What is a derivation?
    * How do we produce a derivation?
    * How do we transform a derivation?
    * How do we consume a derivation?
* Build products
    * What is a build product?
    * How do we produce a build product?
    * How do we transform a build product?
    * How do we consume a build product?

# What is a Nix expression?

A Nix expressions is a purely functional code written in the Nix language

A Nix expression may `import` other Nix expressions stored within files

Reading a Nix expression from a file is a special case of `import`

For example, these two commands are identical:

```
$ nix-instantiate example.nix
$ nix-instantiate --expr 'import ./example.nix'
```

Strings, not files, are the universal interface to the Nix programming language

What are some contexts where we can evaluate a Nix expression without a file?

# Producing a Nix expression

Anything that can produce a string can produce a Nix expression, including:

* Text editors
* Command lines (such as `nix-repl` or `--expr` arguments to Nix commands)
* Nix expressions!
    * This is known as "import from derivation" (or "IFD" for short)
* Check out text files from a `git` repository
    * You can combine this with import from derivation

Not covered in this talk:

* `nix-channel`s
    * I discourage the use of channels for commercial development
* HTML form input
    * Example: Hydra web user interface


# Example: text editor

Use your editor to save this expression to a file named `add.nix`:

```nix
2 + 2
```

Then evaluate the expression:

```bash
$ nix-instantiate --eval add.nix
4
```

# Example: string literal

```bash
$ nix-instantiate --eval --expr '2 + 2'
4
$ nix-instantiate --eval --expr 'import ./add.nix'
4
```

```bash
$ nix-repl
Welcome to Nix version 1.11.4. Type :? for help.

nix-repl> 2 + 2
4

```

# Example: import from derivation

```bash
$ cat derivation.nix
let
  pkgs = import <nixpkgs> { };

in
  pkgs.writeText "result.nix" "2 + 2"

$ nix-instantiate --read-write-mode --eval --expr 'let derivation = import ./derivation.nix; in import derivation'
building path(s) ‘/nix/store/0nskx7yj8glb2kbqa1lz5alvw4ngz25a-result.nix’
4

$ cat /nix/store/0nskx7yj8glb2kbqa1lz5alvw4ngz25a-result.nix
2 + 2
```

# Example: obtaining `nixpkgs` using import from derivation

[Awake Security](https://awakesecurity.com/) open sourced a derivation for
retrieving `nixpkgs`:

* https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH

```nix
{ rev                             # The Git revision of nixpkgs to fetch
, sha256                          # The SHA256 of the downloaded data
, system ? builtins.currentSystem # This is overridable if necessary
}:

with {
  ifThenElse = { bool, thenValue, elseValue }: (
    if bool then thenValue else elseValue);
};

ifThenElse {
  bool = (0 <= builtins.compareVersions builtins.nixVersion "1.12");

  # In Nix 1.12, we can just give a `sha256` to `builtins.fetchTarball`.
  thenValue = (
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256;
    });

  # This hack should at least work for Nix 1.11
  elseValue = (
    (rec {
      tarball = import <nix/fetchurl.nix> {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        inherit sha256;
      };

      builtin-paths = import <nix/config.nix>;
      
      script = builtins.toFile "nixpkgs-unpacker" ''
        "$coreutils/mkdir" "$out"
        cd "$out"
        "$gzip" --decompress < "$tarball" | "$tar" -x --strip-components=1
      '';

      nixpkgs = builtins.derivation {
        name = "nixpkgs-${builtins.substring 0 6 rev}";

        builder = builtins.storePath builtin-paths.shell;

        args = [ script ];

        inherit tarball system;

        tar       = builtins.storePath builtin-paths.tar;
        gzip      = builtins.storePath builtin-paths.gzip;
        coreutils = builtins.storePath builtin-paths.coreutils;
      };
    }).nixpkgs);
}
```

# Example: obtaining `nixpkgs` using import from derivation

You can import from a `nixpkgs` derivation instead of a `<nixpkgs>` channel:

```nix
let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
     rev = "76d649b59484607901f0c1b8f737d8376a904019";
     sha256 = "01c2f4mj4ahir0sxk9kxbymg2pki1pc9a3y6r9x6ridry75fzb8h";
  };

  pkgs = import nixpkgs { config = {}; };

in
  pkgs.hello
```

```bash
$ nix-build hello.nix
building path(s) ‘/nix/store/likfhl38gcaxl5s6zywlspaln32lqa2x-nixpkgs-76d649’
/nix/store/4bc0vx8j66wrm6g44qsswmy060k61qnh-hello-2.10
```

This is better for commercial development where perfect reproducibility matters

# Transforming a Nix expression

The main way to transform a Nix expression is to evaluate the expression

Main ways to evaluate an expression:

* `nix-instantiate --eval`
* `nix-repl`
* implicitly as part of another command (such as `nix-build`)

# Example: `nix-instantiate`

```bash
$ nix-instantiate --eval --expr '2 + 2'
4
```

Evaluating a derivation:

```bash
$ nix-instantiate --eval --expr 'let pkgs = import <nixpkgs> { }; in pkgs.hello'
{ __ignoreNulls = true; __impureHostDeps = <CODE>; __propagatedImpureHostDeps = 
<CODE>; __propagatedSandboxProfile = <CODE>; __sandboxProfile = <CODE>; all = <C
ODE>; args = <CODE>; buildInputs = <CODE>; builder = <CODE>; doCheck = true; drv
Attrs = { __ignoreNulls = true; __impureHostDeps = <CODE>; __propagatedImpureHos
tDeps = <CODE>; __propagatedSandboxProfile = <CODE>; __sandboxProfile = <CODE>; 
args = <CODE>; buildInputs = <CODE>; builder = <CODE>; doCheck = true; name = "h
ello-2.10"; nativeBuildInputs = <CODE>; propagatedBuildInputs = <CODE>; propagat
edNativeBuildInputs = <CODE>; src = <CODE>; stdenv = { __impureHostDeps = <CODE>
; __sandboxProfile = <CODE>; all = <CODE>; allowedRequisites = <CODE>; args = <C
...
924537755ce9164fd5b5f81ce16a1c3-src/pkgs/stdenv/generic/setup.sh; shell = <CODE>
; shellPackage = <CODE>; system = "x86_64-darwin"; type = "derivation"; }; syste
m = <CODE>; type = "derivation"; userHook = <CODE>; }
```

A derivation is just a record with field named `type` set to `"derivation"`

# Example: `nix-repl`

```bash
$ nix-repl
Welcome to Nix version 1.11.4. Type :? for help.

nix-repl> 2 + 2
4

nix-repl> let pkgs = import <nixpkgs> { }; in pkgs.hello
«derivation /nix/store/w3a5xqc8zjamz01qqnziwasalbkzyskc-hello-2.10.drv»

nix-repl> let pkgs = import <nixpkgs> { }; in pkgs.hello.type
"derivation"
```

The `nix-repl` automatically shortens derivation records to `«derivation …»`

# Example: implicit evaluation

```bash
$ nix-instantiate --expr 'let pkgs = import <nixpkgs> { }; in pkgs.hello'
…
/nix/store/w3a5xqc8zjamz01qqnziwasalbkzyskc-hello-2.10.drv
```

Here `nix-instantiate` is actually performing two separate steps:

* evaluate the Nix expression to a derivation
* creating the derivation file from the Nix expression

Many Nix commands bundle several orthogonal steps in one invocation

Keep these orthogonal steps mentally separate when reasoning about Nix

# Consuming a Nix expression

The only way to consume a Nix expression is to transform into a derivation

This only works if the Nix expression evaluates to a derivation

i.e. a record with a field named `type` whose value is `"derivation"`

Main ways to transform a Nix expression into a derivation using:

* `nix-instantiate`
* implicitly as part of Nix evaluation
* implicitly as part of another command (such as `nix-build`)

# Example: `nix-instantiate`

You can generate a derivation from the command line using `nix-instantiate`:

```bash
$ nix-instantiate --expr 'let pkgs = import <nixpkgs> { }; in pkgs.hello'
…
/nix/store/w3a5xqc8zjamz01qqnziwasalbkzyskc-hello-2.10.drv
```

# Example: implicit as part of Nix evaluation

Nix automatically generates derivations that the root derivation references:

For example, the `both` derivation references `hello` and `goodbye` derivations:

```nix
let
  pkgs = import <nixpkgs> { };

  hello = pkgs.writeText "hello.txt" ''
    Hello, world!
  '';

  goodbye = pkgs.writeText "goodbye.txt" ''
    Goodbye, world!
  '';

  both = pkgs.runCommand "both.txt" {} ''
    ${pkgs.coreutils}/bin/cp ${hello} ${goodbye}
  '';

in
  both
```

# Example: implicit as part of Nix evaluation

Generating the `both` derivation also generates the `hello`/`goodbye`
derivations:

```bash
$ nix-instantiate both.nix 
…
/nix/store/a67d2b60vx30isvpgfnyk4qap92sn7zq-both.txt.drv

$ pretty-derivation < /nix/store/a67d2b60vx30isvpgfnyk4qap92sn7zq-both.txt.drv
Derivation
  { …
  , inputDrvs =
      fromList
        [ …
        , ( FilePath
              "/nix/store/f37sv45m6b3nhxw7hk1jd2iqyhqaq7bp-hello.txt.drv"
          , fromList [ "out" ]
          )
        , ( FilePath
              "/nix/store/h1phr2qq4as23rd9dgy5ccqg4ysdf0lf-goodbye.txt.drv"
          , fromList [ "out" ]
          )
        , …
        ]
  , …
  }

$ test -e /nix/store/f37sv45m6b3nhxw7hk1jd2iqyhqaq7bp-hello.txt.drv && echo $?
0

$ test -e /nix/store/h1phr2qq4as23rd9dgy5ccqg4ysdf0lf-goodbye.txt.drv && echo $?
0
```

# Example: implicit as part of another command

If you take a `nix-instantiate` command:

```bash
$ nix-instantiate --expr 'let pkgs = import <nixpkgs> { }; in pkgs.hello'
…
/nix/store/w3a5xqc8zjamz01qqnziwasalbkzyskc-hello-2.10.drv
```

... and replace `nix-instantiate` with `nix-build`:

```bash
$ nix-build --expr 'let pkgs = import <nixpkgs> { }; in pkgs.hello'
/nix/store/h5paliil3r6m70na37ymba1f007mm28k-hello-2.10
```

... then Nix generates the derivation *and* builds the corresponding product

# Overview

* Source code
    * What are Nix expressions?
    * How do we produce Nix expressions?
    * How do we consume Nix expressions?
    * How do we transform nix expressions?
* Derivations
    * What is a derivation?
    * How do we produce a derivation?
    * How do we consume a derivation?
    * How do we transform a derivation?
* Build products
    * What is a build product?
    * How do we produce a build product?
    * How do we consume a build product?
    * How do we transform a build product?

# What is a derivation?

A derivation is a language-independent recipe for how to build a path

* "Language-independent" means derivations are also Nix-independent
* A "path" you build can be a single file or a directory tree

Derivations strive to be "reproducible" and "deterministic"

* "Reproducible" means that you can always build the derivation from scratch
* "Deterministic" means that the result is always bit-for-bit identical

They are not perfectly reproducible/deterministic in practice, but better than
most alternatives

# Derivation files

Derivations are stored as `*.drv` files underneath the `/nix/store` directory:

```shell
$ find /nix/store -name '*.drv' | head
/nix/store/0008hdcdvkrr5mcqahy416hv6rmb5fwg-void-0.7.1.tar.gz.drv
/nix/store/000p9frz8wf8sns2jhn0bj94hl7ksdyq-bash43-030.drv
/nix/store/001d062xhg7bvypci1pa7h5x4ml5kz72-http-client-tls-0.3.5.1.tar.gz.drv
/nix/store/003cbxwyv9k2ihpxygxn523kxjlkpjbd-data-default-instances-dlist-0.0.1.drv
/nix/store/00548b6iwgwrrhblbyv3xqg15ycq8rcp-expat-2.2.2.drv
/nix/store/005bxi36hyy4hq889qbgkks8rv28l769-asciidoc-8.6.9.drv
/nix/store/005lcsd2xlczr9pp30qdgnml636vjz9r-gawk-4.1.4.drv
/nix/store/005vswmy4cjyjprzjjw5gmjvbll60zn2-x509-1.6.5.drv
/nix/store/00742vvlcwi00cryiwm24i75y43jvjan-dbus-conf.drv
/nix/store/0077zd7wg4rc38nppn0als642yp5sdif-Jinja2-2.8.tar.gz.drv
```

# Derivation format

Derivations are serialized as one long line of text:

```shell
$ fold /nix/store/0008hdcdvkrr5mcqahy416hv6rmb5fwg-void-0.7.1.tar.gz.drv
Derive([("out","/nix/store/fbbqa4x05q9x0w6s1fqmx7k676d2zyz1-void-0.7.1.tar.gz","
sha256","c9f0fd93680c029abb9654b5464be260652829961b18b7046f96a0df95e825f4")],[("
/nix/store/cwnn2alfww3six2ywph5hnnlmxwhv9c7-curl-7.52.1.drv",["dev"]),("/nix/sto
re/kzs0g1ch3a59ar14xnms1wj22p2bnr9l-stdenv.drv",["out"]),("/nix/store/qq7pqyfn98
314fd30xspb1hi3rqda2lh-bash-4.3-p48.drv",["out"]),("/nix/store/r1b0rbna957biiy63
m75yxsw3aphps9b-mirrors-list.drv",["out"])],["/nix/store/5pqfb6ik1cxqq1d0irlx306
0jx1qjmsn-builder.sh"],"x86_64-linux","/nix/store/gabjbkwga2dhhp2wzyaxl83r8hjjfc
37-bash-4.3-p48/bin/bash",["-e","/nix/store/5pqfb6ik1cxqq1d0irlx3060jx1qjmsn-bui
lder.sh"],[("buildInputs",""),("builder","/nix/store/gabjbkwga2dhhp2wzyaxl83r8hj
jfc37-bash-4.3-p48/bin/bash"),("curlOpts",""),("downloadToTemp",""),("executable
",""),("impureEnvVars","http_proxy https_proxy ftp_proxy all_proxy no_proxy NIX_
CURL_FLAGS NIX_HASHED_MIRRORS NIX_CONNECT_TIMEOUT NIX_MIRRORS_apache NIX_MIRRORS
_bioc NIX_MIRRORS_bitlbee NIX_MIRRORS_cpan NIX_MIRRORS_debian NIX_MIRRORS_fedora
 NIX_MIRRORS_gcc NIX_MIRRORS_gentoo NIX_MIRRORS_gnome NIX_MIRRORS_gnu NIX_MIRROR
S_gnupg NIX_MIRRORS_hackage NIX_MIRRORS_hashedMirrors NIX_MIRRORS_imagemagick NI
X_MIRRORS_kde NIX_MIRRORS_kernel NIX_MIRRORS_metalab NIX_MIRRORS_mozilla NIX_MIR
RORS_mysql NIX_MIRRORS_oldsuse NIX_MIRRORS_openbsd NIX_MIRRORS_opensuse NIX_MIRR
ORS_postgresql NIX_MIRRORS_pypi NIX_MIRRORS_roy NIX_MIRRORS_sagemath NIX_MIRRORS
_samba NIX_MIRRORS_savannah NIX_MIRRORS_sourceforge NIX_MIRRORS_sourceforgejp NI
X_MIRRORS_steamrt NIX_MIRRORS_ubuntu NIX_MIRRORS_xfce NIX_MIRRORS_xorg"),("mirro
rsFile","/nix/store/ab4zh0ga99y5xj441arp89zl8s4jfc7y-mirrors-list"),("name","voi
d-0.7.1.tar.gz"),("nativeBuildInputs","/nix/store/3ngwsbzhibvc434nqwq6jph6w7c2wa
s6-curl-7.52.1-dev"),("out","/nix/store/fbbqa4x05q9x0w6s1fqmx7k676d2zyz1-void-0.
7.1.tar.gz"),("outputHash","c9f0fd93680c029abb9654b5464be260652829961b18b7046f96
a0df95e825f4"),("outputHashAlgo","sha256"),("outputHashMode","flat"),("postFetch
",""),("preferHashedMirrors","1"),("preferLocalBuild","1"),("propagatedBuildInpu
ts",""),("propagatedNativeBuildInputs",""),("showURLs",""),("stdenv","/nix/store
/985d95clq0216a6pcp3qzw4igp84ajvr-stdenv"),("system","x86_64-linux"),("urls","mi
rror://hackage/void-0.7.1.tar.gz")])
```

# Pretty-printing derivations

You can pretty-print derivations using `ppsh`

You can install `ppsh` from Haskell's `pretty-show` package

```shell
$ nix-shell --packages haskellPackages.pretty-show
$ ppsh < /nix/store/0008hdcdvkrr5mcqahy416hv6rmb5fwg-void-0.7.1.tar.gz.drv
Derive
  ( [ ( "out"
      , "/nix/store/fbbqa4x05q9x0w6s1fqmx7k676d2zyz1-void-0.7.1.tar.gz"
      , "sha256"
      , "c9f0fd93680c029abb9654b5464be260652829961b18b7046f96a0df95e825f4"
      )
    ]
  , [ ( "/nix/store/cwnn2alfww3six2ywph5hnnlmxwhv9c7-curl-7.52.1.drv"
      , [ "dev" ]
      )
    , ( "/nix/store/kzs0g1ch3a59ar14xnms1wj22p2bnr9l-stdenv.drv"
      , [ "out" ]
      )
    , ( "/nix/store/qq7pqyfn98314fd30xspb1hi3rqda2lh-bash-4.3-p48.drv"
      , [ "out" ]
      )
    , ( "/nix/store/r1b0rbna957biiy63m75yxsw3aphps9b-mirrors-list.drv"
      , [ "out" ]
      )
    ]
  , [ "/nix/store/5pqfb6ik1cxqq1d0irlx3060jx1qjmsn-builder.sh" ]
  , "x86_64-linux"
  , "/nix/store/gabjbkwga2dhhp2wzyaxl83r8hjjfc37-bash-4.3-p48/bin/bash"
  , [ "-e"
    , "/nix/store/5pqfb6ik1cxqq1d0irlx3060jx1qjmsn-builder.sh"
    ]
  , [ ( "buildInputs" , "" )
    , ( "builder"
      , "/nix/store/gabjbkwga2dhhp2wzyaxl83r8hjjfc37-bash-4.3-p48/bin/bash"
      )
    , ( "curlOpts" , "" )
    , ( "downloadToTemp" , "" )
    , ( "executable" , "" )
    , ( "impureEnvVars"
      , "http_proxy https_proxy ftp_proxy all_proxy no_proxy NIX_CURL_FLAGS NIX_HASHED_MIRRORS NIX_CONNECT_TIMEOUT NIX_MIRRORS_apache NIX_MIRRORS_bioc NIX_MIRRORS_bitlbee NIX_MIRRORS_cpan NIX_MIRRORS_debian NIX_MIRRORS_fedora NIX_MIRRORS_gcc NIX_MIRRORS_gentoo NIX_MIRRORS_gnome NIX_MIRRORS_gnu NIX_MIRRORS_gnupg NIX_MIRRORS_hackage NIX_MIRRORS_hashedMirrors NIX_MIRRORS_imagemagick NIX_MIRRORS_kde NIX_MIRRORS_kernel NIX_MIRRORS_metalab NIX_MIRRORS_mozilla NIX_MIRRORS_mysql NIX_MIRRORS_oldsuse NIX_MIRRORS_openbsd NIX_MIRRORS_opensuse NIX_MIRRORS_postgresql NIX_MIRRORS_pypi NIX_MIRRORS_roy NIX_MIRRORS_sagemath NIX_MIRRORS_samba NIX_MIRRORS_savannah NIX_MIRRORS_sourceforge NIX_MIRRORS_sourceforgejp NIX_MIRRORS_steamrt NIX_MIRRORS_ubuntu NIX_MIRRORS_xfce NIX_MIRRORS_xorg"
      )
    , ( "mirrorsFile"
      , "/nix/store/ab4zh0ga99y5xj441arp89zl8s4jfc7y-mirrors-list"
      )
    , ( "name" , "void-0.7.1.tar.gz" )
    , ( "nativeBuildInputs"
      , "/nix/store/3ngwsbzhibvc434nqwq6jph6w7c2was6-curl-7.52.1-dev"
      )
    , ( "out"
      , "/nix/store/fbbqa4x05q9x0w6s1fqmx7k676d2zyz1-void-0.7.1.tar.gz"
      )
    , ( "outputHash"
      , "c9f0fd93680c029abb9654b5464be260652829961b18b7046f96a0df95e825f4"
      )
    , ( "outputHashAlgo" , "sha256" )
    , ( "outputHashMode" , "flat" )
    , ( "postFetch" , "" )
    , ( "preferHashedMirrors" , "1" )
    , ( "preferLocalBuild" , "1" )
    , ( "propagatedBuildInputs" , "" )
    , ( "propagatedNativeBuildInputs" , "" )
    , ( "showURLs" , "" )
    , ( "stdenv"
      , "/nix/store/985d95clq0216a6pcp3qzw4igp84ajvr-stdenv"
      )
    , ( "system" , "x86_64-linux" )
    , ( "urls" , "mirror://hackage/void-0.7.1.tar.gz" )
    ]
  )
```

# Haskell API for derivations

I authored the `nix-derivation` Haskell package for working with derivations

This package provides a `pretty-derivation` executable for pretty output:

```shell
$ pretty-derivation < /nix/store/0008hdcdvkrr5mcqahy416hv6rmb5fwg-void-0.7.1.tar.gz.drv
Derivation
  { outputs =
      fromList
        [ ( "out"
          , DerivationOutput
              { path =
                  FilePath
                    "/nix/store/fbbqa4x05q9x0w6s1fqmx7k676d2zyz1-void-0.7.1.tar.gz"
              , hashAlgo = "sha256"
              , hash =
                  "c9f0fd93680c029abb9654b5464be260652829961b18b7046f96a0df95e825f4"
              }
          )
        ]
  , inputDrvs =
      fromList
        [ ( FilePath
              "/nix/store/cwnn2alfww3six2ywph5hnnlmxwhv9c7-curl-7.52.1.drv"
          , fromList [ "dev" ]
          )
        , ( FilePath
              "/nix/store/kzs0g1ch3a59ar14xnms1wj22p2bnr9l-stdenv.drv"
          , fromList [ "out" ]
          )
        , ( FilePath
              "/nix/store/qq7pqyfn98314fd30xspb1hi3rqda2lh-bash-4.3-p48.drv"
          , fromList [ "out" ]
          )
        , ( FilePath
              "/nix/store/r1b0rbna957biiy63m75yxsw3aphps9b-mirrors-list.drv"
          , fromList [ "out" ]
          )
        ]
  , inputSrcs =
      fromList
        [ FilePath "/nix/store/5pqfb6ik1cxqq1d0irlx3060jx1qjmsn-builder.sh"
        ]
  , platform = "x86_64-linux"
  , builder =
      "/nix/store/gabjbkwga2dhhp2wzyaxl83r8hjjfc37-bash-4.3-p48/bin/bash"
  , args =
      [ "-e" , "/nix/store/5pqfb6ik1cxqq1d0irlx3060jx1qjmsn-builder.sh" ]
  , env =
      fromList
        [ ( "buildInputs" , "" )
        , ( "builder"
          , "/nix/store/gabjbkwga2dhhp2wzyaxl83r8hjjfc37-bash-4.3-p48/bin/bash"
          )
        , ( "curlOpts" , "" )
        , ( "downloadToTemp" , "" )
        , ( "executable" , "" )
        , ( "impureEnvVars"
          , "http_proxy https_proxy ftp_proxy all_proxy no_proxy NIX_CURL_FLAGS NIX_HASHED_MIRRORS NIX_CONNECT_TIMEOUT NIX_MIRRORS_apache NIX_MIRRORS_bioc NIX_MIRRORS_bitlbee NIX_MIRRORS_cpan NIX_MIRRORS_debian NIX_MIRRORS_fedora NIX_MIRRORS_gcc NIX_MIRRORS_gentoo NIX_MIRRORS_gnome NIX_MIRRORS_gnu NIX_MIRRORS_gnupg NIX_MIRRORS_hackage NIX_MIRRORS_hashedMirrors NIX_MIRRORS_imagemagick NIX_MIRRORS_kde NIX_MIRRORS_kernel NIX_MIRRORS_metalab NIX_MIRRORS_mozilla NIX_MIRRORS_mysql NIX_MIRRORS_oldsuse NIX_MIRRORS_openbsd NIX_MIRRORS_opensuse NIX_MIRRORS_postgresql NIX_MIRRORS_pypi NIX_MIRRORS_roy NIX_MIRRORS_sagemath NIX_MIRRORS_samba NIX_MIRRORS_savannah NIX_MIRRORS_sourceforge NIX_MIRRORS_sourceforgejp NIX_MIRRORS_steamrt NIX_MIRRORS_ubuntu NIX_MIRRORS_xfce NIX_MIRRORS_xorg"
          )
        , ( "mirrorsFile"
          , "/nix/store/ab4zh0ga99y5xj441arp89zl8s4jfc7y-mirrors-list"
          )
        , ( "name" , "void-0.7.1.tar.gz" )
        , ( "nativeBuildInputs"
          , "/nix/store/3ngwsbzhibvc434nqwq6jph6w7c2was6-curl-7.52.1-dev"
          )
        , ( "out"
          , "/nix/store/fbbqa4x05q9x0w6s1fqmx7k676d2zyz1-void-0.7.1.tar.gz"
          )
        , ( "outputHash"
          , "c9f0fd93680c029abb9654b5464be260652829961b18b7046f96a0df95e825f4"
          )
        , ( "outputHashAlgo" , "sha256" )
        , ( "outputHashMode" , "flat" )
        , ( "postFetch" , "" )
        , ( "preferHashedMirrors" , "1" )
        , ( "preferLocalBuild" , "1" )
        , ( "propagatedBuildInputs" , "" )
        , ( "propagatedNativeBuildInputs" , "" )
        , ( "showURLs" , "" )
        , ( "stdenv"
          , "/nix/store/985d95clq0216a6pcp3qzw4igp84ajvr-stdenv"
          )
        , ( "system" , "x86_64-linux" )
        , ( "urls" , "mirror://hackage/void-0.7.1.tar.gz" )
        ]
  }
```

# Anatomy of a derivation

The key components of a derivation are:

* Input derivations
    * This is used to compute what needs to be built before this derivation
* The build instructions, consisting of:
    * A command (i.e. `/nix/store/...-bash-4.3-p48/bin/bash`)
    * The command's arguments (i.e. "-e /nix/store/...-builder.sh" ]
    * The command's environment
* The output paths
    * The build succeeds if and only if the command creates all of these paths

# Overview

* **Derivations**
    * What is a derivation?
    * **How do we produce a derivation?**
    * How do we consume a derivation?
* Build products
    * What is a build product?
    * How do we produce a build product?
    * How do we consume a build product?

# Producing derivations: `nix-instantiate`

`nix-instantiate` produces a derivation from a Nix expression:

* input a Nix expression (i.e. `*.nix` file)
* output a derivation (i.e. `/nix/store/*.drv` file)

```shell
$ nix-instantiate --expr '(import <nixpkgs> {}).hello'
/nix/store/w3a5xqc8zjamz01qqnziwasalbkzyskc-hello-2.10.drv
```

Carefully note: his is the only part that uses the Nix language

# Producing derivations: minimal builder

```c
// touch.c

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {
  char *out;
  FILE *fp;

  out = getenv("out");

  fp = fopen(out, "w");

  fclose(fp);
}
```

```shell
$ gcc ./touch.c -o ./touch
```

# Producing derivations: minimal derivation

Required reading: https://nixos.org/nix/manual/#ssec-derivation

```nix
# empty.nix

derivation {
  name = "empty";

  system = "x86_64-darwin";

  # Same as: builder = /nix/store/j65p7rrvmk6zhhbn19il02gsfrwcsgf9-touch;
  builder = ./touch;
}
```

```shell
$ nix-instantiate empty.nix
/nix/store/21wp6p5cd55kwf6f5p91w2ac031ngyv5-empty.drv
pretty-derivation < /nix/store/21wp6p5cd55kwf6f5p91w2ac031ngyv5-empty.drv 
Derivation
  { outputs =
      fromList
        [ ( "out"
          , DerivationOutput
              { path =
                  FilePath "/nix/store/wm2xkgrf072h2rkgdbaym700rvrgvrp0-empty"
              , hashAlgo = ""
              , hash = ""
              }
          )
        ]
  , inputDrvs = fromList []
  , inputSrcs =
      fromList
        [ FilePath "/nix/store/j65p7rrvmk6zhhbn19il02gsfrwcsgf9-touch" ]
  , platform = "x86_64-darwin"
  , builder = "/nix/store/j65p7rrvmk6zhhbn19il02gsfrwcsgf9-touch"
  , args = []
  , env =
      fromList
        [ ( "builder"
          , "/nix/store/j65p7rrvmk6zhhbn19il02gsfrwcsgf9-touch"
          )
        , ( "name" , "empty" )
        , ( "out" , "/nix/store/wm2xkgrf072h2rkgdbaym700rvrgvrp0-empty" )
        , ( "system" , "x86_64-darwin" )
        ]
  }
```

# Producing derivations: `nix-store --add`

Note that this approach is **not** safe:

```shell
$ cat > empty.drv <<EOF
Derive([("out","/nix/store/wm2xkgrf072h2rkgdbaym700rvrgvrp0-empty","","")],[],["/nix/store/j65p7rrvmk6zhhbn19il02gsfrwcsgf9-touch"],"x86_64-darwin","/nix/store/j65p7rrvmk6zhhbn19il02gsfrwcsgf9-touch",[],[("builder","/nix/store/j65p7rrvmk6zhhbn19il02gsfrwcsgf9-touch"),("name","empty"),("out","/nix/store/wm2xkgrf072h2rkgdbaym700rvrgvrp0-empty"),("system","x86_64-darwin")])
EOF
```

```shell
$ nix-store --add empty.drv 
/nix/store/c8k1v7k7w349pz5lin1234fh2vhd394l-empty.drv
```

# Overview

* **Derivations**
    * What is a derivation?
    * How do we produce a derivation?
    * **How do we consume a derivation?**
* Build products
    * What is a build product?
    * How do we produce a build product?
    * How do we consume a build product?

# Consuming derivations: `nix-store --realise`

You can use `nix-store --realise` to build a derivation:

For each derivation output, Nix will:

* Check if the build product already exists
* If not, then check if the build product can be retrieved from a cache
* If not, then build the derivation

```shell
$ nix-store --realise /nix/store/c8k1v7k7w349pz5lin1234fh2vhd394l-empty.drv
/nix/store/wm2xkgrf072h2rkgdbaym700rvrgvrp0-empty
```

```shell
$ nix-store --realise /nix/store/0008hdcdvkrr5mcqahy416hv6rmb5fwg-void-0.7.1.tar.gz.drv 
/nix/store/fbbqa4x05q9x0w6s1fqmx7k676d2zyz1-void-0.7.1.tar.gz
```

```shell
$ nix-store --realise /nix/store/w3a5xqc8zjamz01qqnziwasalbkzyskc-hello-2.10.drv
/nix/store/h5paliil3r6m70na37ymba1f007mm28k-hello-2.10
```

# TODO

* Explain how various command lines desugar to Nix expressions trings
    * i.e. the equivalent `--expr`
* Explain why channels are not covered
* Explain why `NIX_PATH` is not covered
