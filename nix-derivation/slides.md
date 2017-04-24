% How to write a Nix derivation
% Gabriel Gonzalez
% April 18, 2017

# Goals

**REMINDER: RECORD THIS TALK!**

This talk will cover how to author a Nix derivation to build a package

I will translate `make` idioms to Nix idioms and compare and contrast the two

**By the end of this talk you should be able to translate a `Makefile` to Nix**

# Overview

* **Nix derivation basics**
* Nix standard environment
* Conclusion

# Minimal derivation

A Nix derivation is a Bash script that creates a file or directory at `$out`

The following minimal Nix derivation creates an empty file:

```haskell
let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "example0";

    buildCommand = ''
      touch $out
    '';
  }
```

# Minimal derivation - result

```bash
$ nix-build example0.nix
these derivations will be built:
  /nix/store/y12753lr5dcdg6i50kjg9xg9jc8jas1m-example0.drv
building path(s) ‘/nix/store/mfpc8v8wqq40snm0klzyk372zy429y16-example0’
/nix/store/mfpc8v8wqq40snm0klzyk372zy429y16-example0

$ cat /nix/store/mfpc8v8wqq40snm0klzyk372zy429y16-example0

$ readlink result  # `nix-build` creates a symlink to the result named `result`
/nix/store/mfpc8v8wqq40snm0klzyk372zy429y16-example0

$ cat result

$ nix-build example0.nix  # The result is cached
/nix/store/mfpc8v8wqq40snm0klzyk372zy429y16-example0
```

# Minimal derivation - `make`

Compare with how we would define this in a `Makefile`

```make
example0:
    touch example0
```

`Makefile`s are much simpler!

`Makefile`s don't enforce that the command produces the declared product

For example, this is legal:

```make
example0:
    touch example1
```

This will keep "succeeding" with a false positive and never trigger a cache hit

# Non-empty file

Let's create a non-empty file:

```haskell
let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "hello.txt";

    buildCommand = ''
      echo 'Hello, world!' > $out
    '';
  }
```

# Non-empty file - result

```bash
$ nix-build file.nix 
these derivations will be built:
  /nix/store/r1jrs6fxw2gk3dar4z48vkg5qx8vgxfp-hello.txt.drv
building path(s) ‘/nix/store/f5vjkqw8hnh9wzyjfi98xhr8l4rc6ry3-hello.txt’
/nix/store/f5vjkqw8hnh9wzyjfi98xhr8l4rc6ry3-hello.txt

$ cat /nix/store/f5vjkqw8hnh9wzyjfi98xhr8l4rc6ry3-hello.txt
Hello, world!

$ cat result
Hello, world!
```

# File - Compare to `make`

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
let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "hello.txt";

    buildCommand = ''
      echo 'Goodbye, world!' > $out
    '';
  }
```

... then we trigger a rebuild:

```bash
$ nix-build file.nix 
these derivations will be built:
  /nix/store/0r4pwm0f7508ab6l11h63hhn8cg2b8rv-hello.txt.drv
building path(s) ‘/nix/store/2zf7pav7gmlq68qd71rw3nzbg1aj8dzc-hello.txt’
/nix/store/2zf7pav7gmlq68qd71rw3nzbg1aj8dzc-hello.txt
```

# Directory

The output of a derivation can be a file or a directory

Whatever you save at the path `$out` is what gets stored in the `/nix/store`

```haskell
let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "directory";

    buildCommand = ''
      mkdir $out
      echo 'Hello!'   > $out/hello.txt
      echo 'Goodbye!' > $out/goodbye.txt
    '';
  }
```

# Directory - result

```haskell
$ nix-build directory.nix
these derivations will be built:
  /nix/store/mylmk5pvlm0qslvrv03z86x2zqj8xxq6-directory.drv
building path(s) ‘/nix/store/wghprg0ifjq29rv6k54n7s0qqnamqhj4-directory’
/nix/store/wghprg0ifjq29rv6k54n7s0qqnamqhj4-directory

$ ls /nix/store/wghprg0ifjq29rv6k54n7s0qqnamqhj4-directory
goodbye.txt	hello.txt

$ cat /nix/store/wghprg0ifjq29rv6k54n7s0qqnamqhj4-directory/hello.txt
Hello!

$ cat /nix/store/wghprg0ifjq29rv6k54n7s0qqnamqhj4-directory/goodbye.txt
Goodbye!

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
$ nix-build -A libiconv '<nixpkgs>'
/nix/store/65xh2q7v9mgbyqkyn1h7imaqkjyq4szq-libiconv-osx-10.9.5
$ tree /nix/store/65xh2q7v9mgbyqkyn1h7imaqkjyq4szq-libiconv-osx-10.9.5
/nix/store/65xh2q7v9mgbyqkyn1h7imaqkjyq4szq-libiconv-osx-10.9.5
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
└── share
    ├── doc
    │   ├── iconv.1.html
    │   ├── iconv.3.html
    │   ├── iconv_close.3.html
    │   ├── iconv_open.3.html
    │   └── iconvctl.3.html
    └── man
        └── man1
            └── iconv.1.gz

7 directories, 20 files
```

# Interpolation

You can reference the paths of other derivations:

```haskell
let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "slides.html";

    buildCommand = ''
      mkdir $out
      ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./slides.md} -o $out/slides.html
    '';
  }
```

A file is also treated as a derivation that adds that file to the `/nix/store`

# Interpolation - result

```bash
$ nix-build slides.nix
these derivations will be built:
  /nix/store/4n0lvh938d6p4fim0f8jdrh2sfgamikn-slides.html.drv
building path(s) ‘/nix/store/hlqb1xj87vmf094nx3cx4jiyi5419aa3-slides.html’
/nix/store/hlqb1xj87vmf094nx3cx4jiyi5419aa3-slides.html
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
let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "impure";

    buildCommand = ''
      date > $out
    '';
  }
```

This derivation is not deterministic!

# Determinism - result

```bash
$ nix-build impure.nix
Gabriels-MacBook-Pro:nix-derivation gabriel$ nix-build impure.nix 
these derivations will be built:
  /nix/store/lhnnn1ci4w149nck13nzaz9sjksj5f58-impure.drv
building path(s) ‘/nix/store/iz1fchxfqcaz2b9nfp3gb40f9bs9m736-impure’
/nix/store/iz1fchxfqcaz2b9nfp3gb40f9bs9m736-impure

$ cat result
Tue Apr 18 21:39:54 UTC 2017

$ nix-build impure.nix  # The rebuild triggers a cache hit!
/nix/store/iz1fchxfqcaz2b9nfp3gb40f9bs9m736-impure

$ cat result  # Still the same result
Tue Apr 18 21:39:54 UTC 2017
```

Nix can't tell that our derivation is not deterministic

Nix assumes by default if the derivation didn't change then result didn't change

# Determinism - Comparison to `make`

`make` uses file timestamps to detect whether or not to rebuild

Nix uses a hash of the derivation to detect whether or not to rebuild

# Questions?

* Nix derivation basics
* **Nix standard environment**
* Conclusion

# Standard environment

Now let's do a more realistic derivation:

```haskell
let
  pkgs = import <nixpkgs> { };

in
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
$ nix-build hello.nix 
these derivations will be built:
  /nix/store/k0p1x6prfw8zj9rg9mjibbl0wkm2y18r-2.10.tar.gz.drv
  /nix/store/wvlmk5zg1748najgc25lsiq2wlyvmym9-hello.drv
building path(s) ‘/nix/store/1n9ylz93pib1283xqy20a0146m4w4vq4-2.10.tar.gz’

trying http://tarballs.nixos.org/sha256/0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
  0     0    0     0    0     0      0      0 --:--:--  0:00:01 --:--:--     0
100  708k  100  708k    0     0   182k      0  0:00:03  0:00:03 --:--:--  545k
building path(s) ‘/nix/store/r624zkk4xsxkv95s1i37s46p473fybfl-hello’
unpacking sources
unpacking source archive /nix/store/1n9ylz93pib1283xqy20a0146m4w4vq4-2.10.tar.gz
source root is hello-2.10
setting SOURCE_DATE_EPOCH to timestamp 1416139241 of file hello-2.10/ChangeLog
patching sources
configuring
configure flags: --disable-dependency-tracking --prefix=/nix/store/r624zkk4xsxkv95s1i37s46p473fybfl-hello  
checking for a BSD-compatible install... /nix/store/dsahcq73w60qbcmf8qdxbfs49v8kg40a-coreutils-8.25/bin/install -c
checking whether build environment is sane... yes
checking for a thread-safe mkdir -p... /nix/store/dsahcq73w60qbcmf8qdxbfs49v8kg40a-coreutils-8.25/bin/mkdir -p
checking for gawk... gawk
checking whether make sets $(MAKE)... yes
...
/nix/store/r624zkk4xsxkv95s1i37s46p473fybfl-hello

$ tree /nix/store/r624zkk4xsxkv95s1i37s46p473fybfl-hello
/nix/store/r624zkk4xsxkv95s1i37s46p473fybfl-hello
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

* Substitute hard-coded paths with `/nix/store` paths
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
checking whether your system is literally a potato
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
/nix/store/dsahcq73w60qbcmf8qdxbfs49v8kg40a-coreutils-8.25/bin/install: cannot remove '/nix/store/r624zkk4xsxkv95s1i37s46p473fybfl-hello/bin/hello': Permission denied
...
```

# Fetching from GitHub

Let's build `redis`!

```haskell
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
$ nix-build redis.nix
these derivations will be built:
  /nix/store/nrf5qk7bpzvmnqxqqnp46gnilwb82hxm-redis-27fe8e9fb2f4adf5337e74280215680e7cd59442-src.drv
  /nix/store/pb47d49p3gkgdzx32r0jsq04xfsb9swx-redis.drv
building path(s) ‘/nix/store/cs4qvm3arwss65x665a4y9f2zlxfa47s-redis-27fe8e9fb2f4adf5337e74280215680e7cd59442-src’

trying https://github.com/antirez/redis/archive/27fe8e9fb2f4adf5337e74280215680e7cd59442.tar.gz
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   155    0   155    0     0    187      0 --:--:-- --:--:-- --:--:--   188
100 1738k    0 1738k    0     0   244k      0 --:--:--  0:00:07 --:--:--  385k
unpacking source archive /private/var/folders/c9/zf_25xbx7bx8yhsxm4q4vw6m0000gn/T/nix-build-redis-27fe8e9fb2f4adf5337e74280215680e7cd59442-s
...
    INSTALL install
    INSTALL install
    INSTALL install
    INSTALL install
    INSTALL install
make[1]: Leaving directory '/private/var/folders/c9/zf_25xbx7bx8yhsxm4q4vw6m0000gn/T/nix-build-redis.drv-0/redis-27fe8e9fb2f4adf5337e74280215680e7cd59442-src/src'
post-installation fixup
patching script interpreter paths in /nix/store/cd3ph0x276kb80iyfmhz8lrc37pnwszf-redis
/nix/store/cd3ph0x276kb80iyfmhz8lrc37pnwszf-redis

$ tree /nix/store/cd3ph0x276kb80iyfmhz8lrc37pnwszf-redis

0 directories, 0 files
```

Oops!

# Fixing redis

The `README` says to use `make PREFIX=/some/other/directory install`

```haskell
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
$ nix-build redis2.nix
...
/nix/store/7cgfazky8jrjia56hi1lxdaf6djhiw4m-redis

$ tree /nix/store/7cgfazky8jrjia56hi1lxdaf6djhiw4m-redis
/nix/store/7cgfazky8jrjia56hi1lxdaf6djhiw4m-redis
└── bin
    ├── redis-benchmark
    ├── redis-check-aof
    ├── redis-check-rdb
    ├── redis-cli
    ├── redis-sentinel -> redis-server
    └── redis-server

1 directory, 6 files

$ result/bin/redis-server 
98937:C 18 Apr 20:20:56.150 # oO0OoO0OoO0Oo Redis is starting oO0OoO0OoO0Oo
98937:C 18 Apr 20:20:56.151 # Redis version=999.999.999, bits=64, commit=00000000, modified=0, pid=98937, just started
98937:C 18 Apr 20:20:56.151 # Warning: no config file specified, using the default config. In order to specify a config file use result/bin/redis-server /path/to/redis.conf
98937:M 18 Apr 20:20:56.151 * Increased maximum number of open files to 10032 (it was originally set to 256).
                _._                                                  
           _.-``__ ''-._                                             
      _.-``    `.  `_.  ''-._           Redis 999.999.999 (00000000/0) 64 bit
  .-`` .-```.  ```\/    _.,_ ''-._                                   
 (    '      ,       .-`  | `,    )     Running in standalone mode
 |`-._`-...-` __...-.``-._|'` _.-'|     Port: 6379
 |    `-._   `._    /     _.-'    |     PID: 98937
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

98937:M 18 Apr 20:20:56.152 # Server initialized
98937:M 18 Apr 20:20:56.153 * DB loaded from disk: 0.000 seconds
98937:M 18 Apr 20:20:56.153 * Ready to accept connections
```

# Pinning `git`

You can use a handy tool called `nix-prefetch-git` to obtain `git` info as JSON

```bash
$ nix-prefetch-git https://github.com/antirez/redis.git > redis.json
Initialized empty Git repository in /private/var/folders/c9/zf_25xbx7bx8yhsxm4q4vw6m0000gn/T/git-checkout-tmp-1yrAOg1g/redis/.git/
remote: Counting objects: 697, done.
remote: Compressing objects: 100% (595/595), done.
remote: Total 697 (delta 110), reused 444 (delta 79), pack-reused 0
Receiving objects: 100% (697/697), 1.89 MiB | 649.00 KiB/s, done.
Resolving deltas: 100% (110/110), done.
From https://github.com/antirez/redis
 * branch            HEAD       -> FETCH_HEAD
Switched to a new branch 'fetchgit'
removing `.git'...

git revision is 27fe8e9fb2f4adf5337e74280215680e7cd59442
path is /nix/store/lv83i05jg78wzasx7hp12iwcsilgj1h8-redis
git human-readable version is -- none --
Commit date is 2017-04-18 16:31:18 +0200
hash is 0ndkqw26p49nh030nj5hgxbd1h8sra9f3hj5asjss3p22a12vjg5

$ cat redis.json
{
  "url": "https://github.com/antirez/redis.git",
  "rev": "27fe8e9fb2f4adf5337e74280215680e7cd59442",
  "date": "2017-04-18T16:31:18+02:00",
  "sha256": "0ndkqw26p49nh030nj5hgxbd1h8sra9f3hj5asjss3p22a12vjg5"
}
```

# Pinning `git`

We can reference this JSON in our Nix derivation:

```haskell
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
$ nix-build redis3.nix
...
/nix/store/33xb2pvywkx1iz5nkikxn4622ksw5ibv-redis
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

# Future topic ideas

Some suggestions:

* Debugging failed derivations
* Writing more complex derivations

Any other ideas for upcoming topics?
