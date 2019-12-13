% How to write a Nix derivation
% Gabriel Gonzalez
% December 12, 2019

# Goals

This talk will cover how to author a Nix derivation to build a package

I will translate `make` idioms to Nix idioms and compare and contrast the two

**By the end of this talk you should be able to translate a `Makefile` to Nix**

# Overview

* **Nix derivation basics**
* Nix standard environment
* Conclusion

# Minimal derivation

A Nix derivation is a script that creates a file or directory at `$out`

The following minimal Nix derivation creates an empty file:

```haskell
# ./example0.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "example0" {} ''
    touch $out
  ''
```

# Minimal derivation - result

```bash
$ nix build --file ./example0.nix
[1 built, 0.0 MiB DL]

$ readlink result  # `nix-build` creates a symlink to the result named `result`
/nix/store/k7bvyxz5rvkjqldrn2g50gk6zxza1455-example0

$ cat result

$ nix build --file ./example0.nix  # The result is cached
```

# Minimal derivation - `make`

Compare with how we would define this in a `Makefile`

```make
example0:
    touch example0
```

The corresponding `Makefile` has less initial overhead

However, `Makefile`s don't ensure that the command produces the declared product

For example, this is legal:

```make
example0:
    touch example1
```

This will keep "succeeding" with a false positive and never trigger a cache hit

Nix makes this mistake impossible (because you don't control the output path)

# Non-empty file

Let's create a non-empty file:

```haskell
# ./file.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "hello.txt" {} ''
    echo 'Hello, world!' > $out
  ''
```

# Non-empty file - result

```bash
$ nix build --file file.nix
[1 built, 0.0 MiB DL]

$ cat result
Hello, world!
```

# Non-empty file - Compare to `make`

```make
hello.txt:
	echo 'Hello, world!' > hello.txt
```

```bash
$ make
echo 'Hello, world!' > hello.txt

$ make
make: `hello.txt' is up to date.
```

... but what happens if we change the `Makefile` to:

```make
hello.txt:
        echo 'Goodbye, world!' > hello.txt
```

```bash
$ make
make: `hello.txt' is up to date.
```

`make` won't rebuild if we change the instructions

# File - Compare to `make`

Nix will rebuild if we change the instructions.  If we change the file to:

```haskell
# ./file.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "goodbye.txt" {} ''
    echo 'Goodbye, world!' > $out
  ''
```

... then we trigger a rebuild:

```bash
$ nix build --file file.nix
[1 built, 0.0 MiB DL]

$ cat result
Goodbye, world!
```

# Directory

The output of a derivation can be a file or a directory

Whatever you save at the path `$out` is what gets stored in the `/nix/store`

```haskell
# ./directory.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "directory" {} ''
    mkdir $out

    echo 'Hello!' > $out/hello.txt

    echo 'Goodbye!' > $out/goodbye.txt
  ''
```

# Directory - result

```haskell
$ nix build --file ./directory.nix 
[1 built, 0.0 MiB DL]

$ ls result
goodbye.txt	hello.txt

$ cat result/hello.txt
Hello!

$ cat result/goodbye.txt
Goodbye!
```

# Directory - compare to `make`

Each `make` recipe only produces a single file

This limits `make`'s utility to building internally within a project

Nix derivations can produce directory trees

This broadens Nix's utility to specify dependencies between multiple projects

For example, here is a derivation that produces an entire package:

```bash
$ nix build --file '<nixpkgs>' libiconv

$ tree result
result
├── bin
│   └── iconv
├── include
│   ├── iconv.h
│   ├── libcharset.h
│   └── localcharset.h
├── lib
│   ├── charset.alias
│   ├── libcharset.1.0.0.dylib
│   ├── libcharset.1.dylib -> libcharset.1.0.0.dylib
│   ├── libcharset.dylib -> libcharset.1.0.0.dylib
│   ├── libcharset.la
│   ├── libiconv-nocharset.dylib -> libiconv.2.4.0.dylib
│   ├── libiconv.2.4.0.dylib
│   ├── libiconv.2.dylib -> libiconv.2.4.0.dylib
│   ├── libiconv.dylib
│   └── libiconv.la
├── nix-support
│   └── setup-hook
└── share
    ├── doc
    │   ├── iconv.1.html
    │   ├── iconv.3.html
    │   ├── iconv_close.3.html
    │   ├── iconv_open.3.html
    │   └── iconvctl.3.html
    └── man
        ├── man1
        │   └── iconv.1.gz
        └── man3
            ├── iconv.3.gz
            ├── iconv_close.3.gz
            ├── iconv_open.3.gz
            └── iconvctl.3.gz

9 directories, 25 files
```

# Interpolation

You can reference the paths of other derivations:

```haskell
# ./slides.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "slides.html" {} ''
    mkdir $out

    ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./slides.md} -o $out/slides.html
  ''
```

A file is also treated as a derivation that adds that file to the `/nix/store`

# Interpolation - result

```bash
$ nix build --file ./slides.nix
[1 built, 0.0 MiB DL]

$ open result/slides.html
```

# Interpolation - compare to `make`

The following derivation is analogous to this `Makefile`:

```make
slides.html: slides.md
	pandoc -t slidy -s slides.md -o slides.html
```

`make` scripts typically don't include executables (like `pandoc`) as dependencies

This means that if we change `pandoc` we won't trigger a rebuild

# Determinism

This derivation stores the current time in `$out`:

```haskell
# ./impure.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "impure" {} ''
    date > $out
  ''
```

This derivation is not deterministic!

# Determinism - result

```bash
$ nix build --file ./impure.nix
[1 built, 0.0 MiB DL]

$ cat result
Tue Dec 10 00:32:31 UTC 2019

$ nix build --file ./impure.nix # The rebuild triggers a cache hit!

$ cat result  # Still the same result
Tue Dec 10 00:32:31 UTC 2019
```

Nix can't tell that our derivation is not deterministic

Nix assumes by default if the derivation didn't change then result didn't change

# Determinism - Comparison to `make`

`make` uses file timestamps to detect whether or not to rebuild

Nix by default uses a hash of the derivation to detect whether or not to rebuild

Nix can optionally use a hash of the result instead (not covered in this talk)

# Questions?

* Nix derivation basics
* **Nix standard environment**
* Conclusion

# Standard environment

Now let's do a more realistic derivation:

```haskell
# ./hello.nix

let
  pkgs = import <nixpkgs> { };

in
  # This is our first time using `pkgs.stdenv.mkDerivation`
  pkgs.stdenv.mkDerivation {
    name = "hello";

    src = pkgs.fetchurl {
      url = "mirror://gnu/hello/2.10.tar.gz";
      sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
    };
  }
```

# Standard environment - result

```bash
$ nix build --file ./hello.nix
[2 built, 16 copied (9.1 MiB), 2.8 MiB DL]

$ nix-store --read-log result
@nix { "action": "setPhase", "phase": "unpackPhase" }
unpacking sources
unpacking source archive /nix/store/1n9ylz93pib1283xqy20a0146m4w4vq4-2.10.tar.gz
source root is hello-2.10
setting SOURCE_DATE_EPOCH to timestamp 1416139241 of file hello-2.10/ChangeLog
@nix { "action": "setPhase", "phase": "patchPhase" }
patching sources
@nix { "action": "setPhase", "phase": "configurePhase" }
configuring
configure flags: --disable-dependency-tracking --prefix=/nix/store/30xjvvcyp1c7psk7h517xpysb3irmyzw-hello
...

$ tree result
result
├── bin
│   └── hello
└── share
    ├── info
    │   └── hello.info
    └── man
        └── man1
            └── hello.1.gz

5 directories, 3 files
```

# Standard environment - explanation

Nix provides a default build script if you don't specify one

This script basically an Autotools-style build (i.e. C/C++ project):

```bash
$ tar xzf $src
$ cd $sourceRoot
$ ./configure --prefix=$out
$ make
$ make check
$ make install
```

I'm oversimplifying, but that gives the general idea

The real implementation is in:

* [https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/generic/setup.sh](https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/generic/setup.sh)

# Customizing the standard build

This default script is customizable to a fault:

* You can add new arguments or flags to every command
* You can inject additional commands before and after each phase
* You can add patches
* You can add or remove phases
* **You can always just throw it away and use your own script**

The default script also has several useful utilities in scope:

* Substitute hard-coded paths (like `/bin/sh`) with `/nix/store` paths
* Automatically unpack any type of archive
* Tweak the runtime environment of built executables

# Stepping through a build

```bash
$ nix-shell hello.nix

$ cd /tmp

$ unpackPhase
unpacking source archive /nix/store/1n9ylz93pib1283xqy20a0146m4w4vq4-2.10.tar.gz
source root is hello-2.10
setting SOURCE_DATE_EPOCH to timestamp 1416139241 of file hello-2.10/ChangeLog

$ cd $sourceRoot

$ patchPhase

$ configurePhase  # ./configure --prefix=$out
...
checking whether your system is a Linux distribution installed on a potato
...

$ buildPhase  # make
...

$ checkPhase  # make check
...
============================================================================
Testsuite summary for GNU Hello 2.10
============================================================================
# TOTAL: 5
# PASS:  4
# SKIP:  1
# XFAIL: 0
# FAIL:  0
# XPASS: 0
# ERROR: 0
============================================================================
...

$ installPhase  # make install (Should fail, because we already installed this)
...
install: cannot remove '/nix/store/30xjvvcyp1c7psk7h517xpysb3irmyzw-hello/bin/hello': Permission denied
...
```

# Fetching from GitHub

Let's build `redis`!

```haskell
# ./redis.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "redis";

    src = pkgs.fetchFromGitHub {
      owner = "antirez";

      repo = "redis";

      rev = "27fe8e9fb2f4adf5337e74280215680e7cd59442";

      sha256 = null;  # We don't know the hash yet, so just put `null`
    };
  }
```

`fetchFromGitHub` is the fastest way to fetch a repository from GitHub

# Fetching from GitHub - result

```bash
$ nix build --file ./redis.nix
builder for '/nix/store/sqn6nb7a00g01mwawibq7wz334yirnf4-redis.drv' failed with exit code 2; last 10 log lines:
  make[1]: Entering directory '/private/var/folders/dt/650nlx5n4cx7jt9mk6w4r2s00000gn/T/nix-build-redis.drv-0/source/src'
      CC Makefile.dep
  
  Hint: It's a good idea to run 'make test' ;)
  
      INSTALL install
  install: cannot create regular file '/usr/local/bin/redis-server': Permission denied
  make[1]: *** [Makefile:280: install] Error 1
  make[1]: Leaving directory '/private/var/folders/dt/650nlx5n4cx7jt9mk6w4r2s00000gn/T/nix-build-redis.drv-0/source/src'
  make: *** [Makefile:9: install] Error 2
[0 built (1 failed)]
error: build of '/nix/store/sqn6nb7a00g01mwawibq7wz334yirnf4-redis.drv' failed
```

Oops!

# Fixing redis

The `README` says to use `make PREFIX=/some/other/directory install`

```haskell
# ./redis.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "redis";

    makeFlags = "PREFIX=$(out)";  # This is one of many flags that we can tweak

    src = pkgs.fetchFromGitHub {
      owner = "antirez";

      repo = "redis";

      rev = "27fe8e9fb2f4adf5337e74280215680e7cd59442";

      sha256 = null;
    };
  }
```

Let's see where that flag is used:

* [https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/generic/setup.sh](https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/generic/setup.sh)

# Fixing redis - result

```bash
$ nix build --file ./redis.nix
[1 built, 0.0 MiB DL]

$ tree result
result
└── bin
    ├── redis-benchmark
    ├── redis-check-aof
    ├── redis-check-rdb
    ├── redis-cli
    ├── redis-sentinel -> redis-server
    └── redis-server

1 directory, 6 files

$ result/bin/redis-server 
51138:C 09 Dec 16:47:02.276 # oO0OoO0OoO0Oo Redis is starting oO0OoO0OoO0Oo
51138:C 09 Dec 16:47:02.277 # Redis version=999.999.999, bits=64, commit=00000000, modified=0, pid=51138, just started
51138:C 09 Dec 16:47:02.277 # Warning: no config file specified, using the default config. In order to specify a config file use result/bin/redis-server /path/to/redis.conf
51138:M 09 Dec 16:47:02.278 * Increased maximum number of open files to 10032 (it was originally set to 256).
                _._                                                  
           _.-``__ ''-._                                             
      _.-``    `.  `_.  ''-._           Redis 999.999.999 (00000000/0) 64 bit
  .-`` .-```.  ```\/    _.,_ ''-._                                   
 (    '      ,       .-`  | `,    )     Running in standalone mode
 |`-._`-...-` __...-.``-._|'` _.-'|     Port: 6379
 |    `-._   `._    /     _.-'    |     PID: 51138
  `-._    `-._  `-./  _.-'    _.-'                                   
 |`-._`-._    `-.__.-'    _.-'_.-'|                                  
 |    `-._`-._        _.-'_.-'    |           http://redis.io        
  `-._    `-._`-.__.-'_.-'    _.-'                                   
 |`-._`-._    `-.__.-'    _.-'_.-'|                                  
 |    `-._`-._        _.-'_.-'    |                                  
  `-._    `-._`-.__.-'_.-'    _.-'                                   
      `-._    `-.__.-'    _.-'                                       
          `-._        _.-'                                           
              `-.__.-'                                               

51138:M 09 Dec 16:47:02.279 # Server initialized
51138:M 09 Dec 16:47:02.279 * Ready to accept connections
```

# Pinning `git`

You can use a handy tool called `nix-prefetch-git` to obtain `git` info as JSON

```bash
$ nix-prefetch-git https://github.com/antirez/redis.git > redis.json
Initialized empty Git repository in /private/var/folders/c9/zf_25xbx7bx8yhsxm4q4vw6m0000gn/T/git-checkout-tmp-VnvwUYA9/redis/.git/
remote: Enumerating objects: 860, done.
remote: Counting objects: 100% (860/860), done.
remote: Compressing objects: 100% (764/764), done.
remote: Total 860 (delta 104), reused 535 (delta 68), pack-reused 0
Receiving objects: 100% (860/860), 2.29 MiB | 4.21 MiB/s, done.
Resolving deltas: 100% (104/104), done.
From https://github.com/antirez/redis
 * branch            HEAD       -> FETCH_HEAD
Switched to a new branch 'fetchgit'
removing `.git'...

git revision is 14045adf9211179687ebd694197643a0fca0116a
path is /nix/store/8ywf860k36a89xywdbrggyi42qgpbag6-redis
git human-readable version is -- none --
Commit date is 2019-12-09 10:41:14 +0100
hash is 03afyqhk9hhxixg1k1b752d34879aaam0wqhxd57kgykxgymmkla

$ cat redis.json 
{
  "url": "https://github.com/antirez/redis.git",
  "rev": "14045adf9211179687ebd694197643a0fca0116a",
  "date": "2019-12-09T10:41:14+01:00",
  "sha256": "03afyqhk9hhxixg1k1b752d34879aaam0wqhxd57kgykxgymmkla",
  "fetchSubmodules": false
}
```

# Pinning `git`

We can reference this JSON in our Nix derivation:

```haskell
# ./redis.nix

let
  pkgs = import <nixpkgs> { };

  json = builtins.fromJSON (builtins.readFile ./redis.json);

in
  pkgs.stdenv.mkDerivation {
    name = "redis";

    makeFlags = "PREFIX=$(out)";

    src = pkgs.fetchFromGitHub {
      owner = "antirez";

      repo = "redis";

      rev = json.rev;

      sha256 = json.sha256;
    };
  }
```

```haskell
$ nix build --file redis.nix
[2 built, 0.0 MiB DL]
```

# Questions?

* Nix derivation basics
* Nix standard environment
* **Conclusion**

# Conclusion

You can use Nix just as a "better `Make`":

Nix fixes many of the design flaws in `make`:

* More accurate rebuild detection
* Better suitability for cross-project builds
* Nix is a real programming language (not covered)

![](awake-blue.png)
