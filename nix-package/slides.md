% Nix package management
% Gabriel Gonzalez
% April 18, 2017

# Goals

This talk will cover how to use Nix to author and manage packages

I will translate `brew` idioms to Nix idioms and compare and contrast the two

**By the end of this talk you should be able to use Nix as a `brew` replacement**

This talk overlaps substantially with the
[Nix manual](http://nixos.org/nix/manual/)

# Overview

* **How to author a minimal Nix package**
* Basic package management
* Development environments
* Our internal use of Nix packages
* Conclusions

# Climbing the abstraction tower

Nix derivations can be used at many abstraction layers:

* Nix derivations can build single file (**LAST BROWNBAG**)
    * i.e. Nix as a `make` replacement
* Nix derivations can build packages (**YOU ARE HERE**)
    * i.e. Nix as a `brew` replacement
* Nix derivations can deploy operating systems and services
    * i.e. Nix as a Debian/`systemd` replacement
* Nix derivations can build containers and virtual machines
    * i.e. Nix as a `docker` replacement
* Nix derivations can deploy clusters of machines/containers
    * i.e. Nix as a `terraform`/`kubernetes` replacement

One benefit of Nix is a single language that spans multiple abstraction layers

# There is no one true way

Nix is very unopinionated about how to manage your development environment

There are varying degrees to which you can "drink the Nix Kool-Aid":

* You can use Nix to install individual development tools globally
* You can use Nix to distribute development tools as a single installable package
* You can use Nix to create transient per-project environments
* Nix can build your project for you

I will cover all of the above workflows in this talk

# What the Backend team does

* We globally install some meta-tools using Nix
    * `nix-prefetch-git`
    * `cabal2nix`
    * `nixops`
    * `delegate`
* Use Nix to create transient and isolated environments when doing development
* Let Nix build the project when pushing to production

In other words, we use Nix more like `virtualenv`/`stack` than `brew`

However, this talk will still cover `brew`-like idioms

# Minimal package

This is a derivation that builds a minimal "Hello, world!" script

```haskell
let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "hello";

    buildCommand = ''
      mkdir -p $out/bin
      cat > $out/bin/hello <<EOF
        echo "Hello, world!"
      EOF
      chmod u+x $out/bin/hello
    '';
  }
```

Carefully note that we place the script in `$out/bin`

# Install our package

You can install any Nix derivation:

```bash
$ nix-env --install --file minimal.nix
installing ‘hello’
```

Anything that the derivation stores in `$out/bin` gets added to your `$PATH`

```haskell
$ hello
Hello, world!
```

# Package layout

Packages can provide other directories:

* `bin/` - for executable programs
* `include/` - for headers
* `lib/` - for object code
* `share/` - for `man`-pages, license information, and static resources
* `etc/` - for configuration

# Multiple packages

We can define multiple packages in the same file using a record:

```haskell
let
  pkgs = import <nixpkgs> { };

in
  { hello = pkgs.stdenv.mkDerivation {
      name = "hello";

      buildCommand = ''
        mkdir -p $out/bin
        cat > $out/bin/hello <<EOF
          echo "Hello, world!"
        EOF
        chmod u+x $out/bin/hello
      '';
    };

    goodbye = pkgs.stdenv.mkDerivation {
      name = "goodbye";

      buildCommand = ''
        mkdir -p $out/bin
        cat > $out/bin/goodbye <<EOF
          echo "Goodbye, world!"
        EOF
        chmod u+x $out/bin/goodbye
      '';
    };
  }
```

# Install package set

Building a record builds all of the fields:

```bash
$ nix-build tools.nix
/nix/store/pk18rsz4isq0w6ah6a12dvfr6dqd88sa-goodbye
/nix/store/h9a2vqq2yz2xmw1cjpsd04bdyvr01rry-hello
```

Similarly, installing a record installs all of the fields:

```bash
$ nix-env --install --file tools.nix 
installing ‘goodbye’
installing ‘hello’
```

# Avoiding repetition

Nix is a programming language, so you can reduce repetition using functions:

```haskell
let
  pkgs = import <nixpkgs> { };

  makeScript = packageName: greeting: pkgs.stdenv.mkDerivation {
    name = packageName;

    buildCommand = ''
      mkdir -p $out/bin
      cat > $out/bin/${packageName} <<EOF
        echo "${greeting}"
      EOF
      chmod u+x $out/bin/${packageName}
    '';
  };

in
  { hello = makeScript "hello" "Hello, world!";

    goodbye = makeScript "goodbye" "Goodbye, world!";
  }
```

# Attribute sets

Nix terminology:

* Nix calls records "attribute sets"
* Nix calls record fields "attributes"
* Nix calls a chain of field accesses an "attribute path"

For example:

* `{ foo = 1; bar = { baz = 2}; }` is an attribute set
* `foo`, `bar`, and `baz` are attributes
* `bar.baz` is an "attribute path"

# Attributes

If you want to build/install a specific "attribute path", use the `--attr` flag:

```bash
$ nix-build --attr hello tools.nix
/nix/store/h9a2vqq2yz2xmw1cjpsd04bdyvr01rry-hello
$ nix-env --install --attr hello --file tools.nix
...
installing ‘hello’
```

# Package repositories

The official Nix package repository is `nixpkgs`

The key file is:

[https://raw.githubusercontent.com/NixOS/nixpkgs/master/pkgs/top-level/all-packages.nix](https://raw.githubusercontent.com/NixOS/nixpkgs/master/pkgs/top-level/all-packages.nix)

This file is a gigantic record containing all packages that you can install

You can install a package from this file using:

```bash
$ nix-env --install --attr hello --file '<nixpkgs>'
```

# Minor digression

Please do not use `nix-env` without the `--file` argument, like this:

```bash
$ nix-env --install --attr hello  # BAD!
```

`nix-env` misbehaves a lot when you do this

You should add `--file '<nixpkgs>'` if you want to install from `nixpkgs`

# Questions?

* How to author a minimal Nix package
* **Basic package management**
* Development environments
* Our internal use of Nix packages
* Conclusions

# Attribute vs. Package Name

There are two ways to install and browse packages:

*   Beginner-friendly: Install and browse packages by **derivation name**
*   Expert-friendly: Install and browse packages by **attribute path**

```haskell
let
  pkgs = import <nixpkgs> { };

in
  # By attribute path
  # ↓
  { hello = pkgs.stdenv.mkDerivation {
      name = "hello";
           # ↑
           # By derivation name

      ...
    };

    goodbye = pkgs.stdenv.mkDerivation {
      name = "goodbye";

      ...
    };
  }
```

So far I've showed you how to install things by **attribute path**

The next few slides will do things by **derivation name**

# Query installed packages

```bash
$ nix-env --query --installed
cabal-install-1.24.0.0
cabal2nix-2.0.4
delegate-1.0.0
dhall-1.0.2
fast-tags-1.3
go2nix-1.1.1
grpc-haskell-0.0.0.0
nix-1.11.6
nix-prefetch-git
nix-repl-1.11.4-2016-02-28
nixops-1.4
packet-analysis-0.1.0.0
rsync-3.1.2
ShellCheck-0.4.4
tree-1.7.0
```

# Query installed packages - `brew`

```bash
$ brew list
ansible20		harfbuzz		openssl
atk			haskell-stack		openvpn
cairo			hello			pango
fontconfig		hicolor-icon-theme	pcre
freetype		icu4c			pixman
gcc			isl			pkg-config
gdk-pixbuf		jpeg			r
gettext			libffi			readline
glib			libmpc			sdl
glpk			libpng			sdl2
gmp			libtiff			ssh-copy-id
gnupg			libyaml			wget
gobject-introspection	lzo			xz
gtk+			mpfr
```

NOTE:

* `brew` lists top-level packages you installed **and their dependencies**
* `nix-env` only lists top-level packages you installed

# Query available packages

```bash
$ nix-env --query --available --file '<nixpkgs>'
a52dec-0.7.4p4
aalib-1.4rc5
abc-1.2.0
...

$ # Get the corresponding attribute path
$ nix-env --query --available --file '<nixpkgs>' --attr-path
a52dec                                                 a52dec-0.7.4p4
aalib                                                  aalib-1.4rc5
abc                                                    abc-1.2.0
...
ocamlPackages.acgtk                                    acgtk-1.1
...

$ # Search for everything with `cabal` in the name
$ nix-env --query --available --file '<nixpkgs>' '.*cabal.*'
all-cabal-hashes-ee101d34ff8bd59897aa2eb0a124bcd3fb47ceec-src
cabal-install-1.24.0.2
cabal2nix-2.0.4
```

# Query available packages - `brew`

```bash
$ brew search
a2ps
a52dec
aacgain
aalib
aamath
aap
...
$ brew search cabal
cabal-install
```

# Install packages

`nix-env`:

```bash
$ nix-env --install cabal-install --file '<nixpkgs>'
```

`brew`:

```bash
$ brew install cabal-install
```

# Uninstall packages

`nix-env`:

```bash
$ nix-env --uninstall cabal-install
```

`brew`:

```bash
$ brew uninstall cabal-install
```

# Uninstall

`brew uninstall` will let you break packages:

```bash
$ brew install gtk
$ brew uninstall glib
$ gtk-demo
dyld: Library not loaded: /usr/local/opt/glib/lib/libgmodule-2.0.0.dylib
  Referenced from: /usr/local/bin/gtk-demo
  Reason: image not found
Trace/BPT trap: 5
```

Nix protects against this mistake

# Profiles

Nix keeps track of your development environment using "profiles":

```
$ readlink ~/.nix-profile
/nix/var/nix/profiles/default
$ readlink /nix/var/nix/profiles/default
default-431-link
$ readlink /nix/var/nix/profiles/default-431-link
/nix/store/fjxgr5pal7c9iv0i7jf738kqcg7cwr8p-user-environment
```

A profile is just a package that links to other packages:

```bash
$ tree /nix/store/fjxgr5pal7c9iv0i7jf738kqcg7cwr8p-user-environment
/nix/store/fjxgr5pal7c9iv0i7jf738kqcg7cwr8p-user-environment
├── Library -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/Library
├── bin
│   ├── analyse-file -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/bin/analyse-file
│   ├── cabal -> /nix/store/a41vdrzivdqi7hn9fslhhmfsrxxlcd7r-cabal-install-1.24.0.0/bin/cabal
│   ├── cabal2nix -> /nix/store/njarvl56b0h8fwz30ifizadiyh3gmj7q-cabal2nix-2.0.4/bin/cabal2nix
│   ├── compile-proto-file -> /nix/store/si7h1b72646yw8rqc1dn0q2kz61x26wa-grpc-haskell-0.0.0.0/bin/compile-proto-file
│   ├── delegate -> /nix/store/wbi1jydszj8vx45jvfwlf6phy3577vzz-delegate-1.0.0/bin/delegate
│   ├── dhall -> /nix/store/xk0ddz8f9f74db6a5bvj38bn5gz4l5gx-dhall-1.0.2/bin/dhall
│   ├── echo-client -> /nix/store/si7h1b72646yw8rqc1dn0q2kz61x26wa-grpc-haskell-0.0.0.0/bin/echo-client
│   ├── echo-server -> /nix/store/si7h1b72646yw8rqc1dn0q2kz61x26wa-grpc-haskell-0.0.0.0/bin/echo-server
│   ├── fast-tags -> /nix/store/azgb01k93yxk91h82cxm6i9mk4z7xwaj-fast-tags-1.3/bin/fast-tags
│   ├── go2nix -> /nix/store/ivr98bmcjz6ni0bp43vfcak8i5b0mdzp-go2nix-1.1.1-bin/bin/go2nix
│   ├── gtk-builder-tool -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/bin/gtk-builder-tool
│   ├── gtk-encode-symbolic-svg -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/bin/gtk-encode-symbolic-svg
│   ├── gtk-launch -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/bin/gtk-launch
│   ├── gtk-query-immodules-3.0 -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/bin/gtk-query-immodules-3.0
│   ├── gtk-query-settings -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/bin/gtk-query-settings
│   ├── gtk-update-icon-cache -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/bin/gtk-update-icon-cache
│   ├── hellos-client -> /nix/store/si7h1b72646yw8rqc1dn0q2kz61x26wa-grpc-haskell-0.0.0.0/bin/hellos-client
│   ├── hellos-server -> /nix/store/si7h1b72646yw8rqc1dn0q2kz61x26wa-grpc-haskell-0.0.0.0/bin/hellos-server
│   ├── nix-build -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-build
│   ├── nix-channel -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-channel
│   ├── nix-collect-garbage -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-collect-garbage
│   ├── nix-copy-closure -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-copy-closure
│   ├── nix-daemon -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-daemon
│   ├── nix-env -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-env
│   ├── nix-generate-patches -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-generate-patches
│   ├── nix-hash -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-hash
│   ├── nix-install-package -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-install-package
│   ├── nix-instantiate -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-instantiate
│   ├── nix-log2xml -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-log2xml
│   ├── nix-prefetch-git -> /nix/store/j5q1by05rlc52prrmmbd0nxab9vyi3bp-nix-prefetch-git/bin/nix-prefetch-git
│   ├── nix-prefetch-url -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-prefetch-url
│   ├── nix-pull -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-pull
│   ├── nix-push -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-push
│   ├── nix-repl -> /nix/store/70svhwysrbvm0fc31f1pp6yg4q1jxfa0-nix-repl-1.11.4-2016-02-28/bin/nix-repl
│   ├── nix-shell -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-shell
│   ├── nix-store -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/bin/nix-store
│   ├── nixops -> /nix/store/hsy0kwjp04rzy4bbajxkb967qppql618-nixops-1.4/bin/nixops
│   ├── packet-analysis -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/bin/packet-analysis
│   ├── packet-analysis-consumer -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/bin/packet-analysis-consumer
│   ├── packet-analysis-meminfo -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/bin/packet-analysis-meminfo
│   ├── pcap-utils -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/bin/pcap-utils
│   ├── proto-file -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/bin/proto-file
│   ├── rsync -> /nix/store/1qcnb87mrk2m8d0wpx9m9awdq8ryyckb-rsync-3.1.2/bin/rsync
│   ├── shellcheck -> /nix/store/sg2pnmp4kg4fj7s5vrk0jmbcxsv0cbrf-ShellCheck-0.4.4/bin/shellcheck
│   ├── stenoproxy -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/bin/stenoproxy
│   ├── tree -> /nix/store/04y5lqwn6967yni50nx1vhsmjdcwm12z-tree-1.7.0/bin/tree
│   └── write-test-datapoint-protobuf -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/bin/write-test-datapoint-protobuf
├── etc
│   ├── bash_completion.d -> /nix/store/a41vdrzivdqi7hn9fslhhmfsrxxlcd7r-cabal-install-1.24.0.0/etc/bash_completion.d
│   ├── gtk-3.0 -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/etc/gtk-3.0
│   ├── profile.d -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/etc/profile.d
│   └── ssl -> /nix/store/8chqc646597kg66z5hzfaljffqwa5khz-nss-cacert-3.28.1/etc/ssl
├── include
│   ├── gail-3.0 -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/include/gail-3.0
│   ├── gtk-3.0 -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/include/gtk-3.0
│   └── nix -> /nix/store/aqjvzzzkfl000kc2b099z3skkynsvf1f-nix-1.11.6-dev/include/nix
├── lib
│   ├── ghc-8.0.1
│   │   ├── ShellCheck-0.4.4 -> /nix/store/sg2pnmp4kg4fj7s5vrk0jmbcxsv0cbrf-ShellCheck-0.4.4/lib/ghc-8.0.1/ShellCheck-0.4.4
│   │   ├── dhall-1.1.0 -> /nix/store/xk0ddz8f9f74db6a5bvj38bn5gz4l5gx-dhall-1.0.2/lib/ghc-8.0.1/dhall-1.1.0
│   │   ├── grpc-haskell-0.0.0.0 -> /nix/store/si7h1b72646yw8rqc1dn0q2kz61x26wa-grpc-haskell-0.0.0.0/lib/ghc-8.0.1/grpc-haskell-0.0.0.0
│   │   └── package.conf.d
│   │       ├── ShellCheck-0.4.4-rJXW2snSywExSDMk0MlFS.conf -> /nix/store/sg2pnmp4kg4fj7s5vrk0jmbcxsv0cbrf-ShellCheck-0.4.4/lib/ghc-8.0.1/package.conf.d/ShellCheck-0.4.4-rJXW2snSywExSDMk0MlFS.conf
│   │       ├── dhall-1.1.0-6M9CWGvs3C8638vEbNxjVQ.conf -> /nix/store/xk0ddz8f9f74db6a5bvj38bn5gz4l5gx-dhall-1.0.2/lib/ghc-8.0.1/package.conf.d/dhall-1.1.0-6M9CWGvs3C8638vEbNxjVQ.conf
│   │       └── grpc-haskell-0.0.0.0-BaMj7UGS8oDCn18Iiyrw4R.conf -> /nix/store/si7h1b72646yw8rqc1dn0q2kz61x26wa-grpc-haskell-0.0.0.0/lib/ghc-8.0.1/package.conf.d/grpc-haskell-0.0.0.0-BaMj7UGS8oDCn18Iiyrw4R.conf
│   ├── ghc-8.0.2
│   │   ├── fast-tags-1.3 -> /nix/store/azgb01k93yxk91h82cxm6i9mk4z7xwaj-fast-tags-1.3/lib/ghc-8.0.2/fast-tags-1.3
│   │   ├── package.conf.d
│   │   │   ├── fast-tags-1.3-Kg8ACNCmPnw2DGOkZ7mdXD.conf -> /nix/store/azgb01k93yxk91h82cxm6i9mk4z7xwaj-fast-tags-1.3/lib/ghc-8.0.2/package.conf.d/fast-tags-1.3-Kg8ACNCmPnw2DGOkZ7mdXD.conf
│   │   │   └── packet-analysis-0.1.0.0-BCRB65RinSACZOGPF10pwL.conf -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/lib/ghc-8.0.2/package.conf.d/packet-analysis-0.1.0.0-BCRB65RinSACZOGPF10pwL.conf
│   │   ├── packet-analysis-0.1.0.0 -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/lib/ghc-8.0.2/packet-analysis-0.1.0.0
│   │   └── x86_64-osx-ghc-8.0.2
│   │       ├── libHSfast-tags-1.3-Kg8ACNCmPnw2DGOkZ7mdXD-ghc8.0.2.dylib -> /nix/store/azgb01k93yxk91h82cxm6i9mk4z7xwaj-fast-tags-1.3/lib/ghc-8.0.2/x86_64-osx-ghc-8.0.2/libHSfast-tags-1.3-Kg8ACNCmPnw2DGOkZ7mdXD-ghc8.0.2.dylib
│   │       └── libHSpacket-analysis-0.1.0.0-BCRB65RinSACZOGPF10pwL-ghc8.0.2.dylib -> /nix/store/6zyfb22lphl6azqxj2xv62k0njm0s9jm-packet-analysis-0.1.0.0/lib/ghc-8.0.2/x86_64-osx-ghc-8.0.2/libHSpacket-analysis-0.1.0.0-BCRB65RinSACZOGPF10pwL-ghc8.0.2.dylib
│   ├── girepository-1.0 -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/girepository-1.0
│   ├── gtk-3.0 -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/gtk-3.0
│   ├── libgailutil-3.0.dylib -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgailutil-3.0.dylib
│   ├── libgailutil-3.dylib -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgailutil-3.dylib
│   ├── libgailutil-3.la -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgailutil-3.la
│   ├── libgdk-3.0.dylib -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgdk-3.0.dylib
│   ├── libgdk-3.dylib -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgdk-3.dylib
│   ├── libgdk-3.la -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgdk-3.la
│   ├── libgtk-3.0.dylib -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgtk-3.0.dylib
│   ├── libgtk-3.dylib -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgtk-3.dylib
│   ├── libgtk-3.la -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/lib/libgtk-3.la
│   ├── libnixexpr.dylib -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/lib/libnixexpr.dylib
│   ├── libnixformat.dylib -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/lib/libnixformat.dylib
│   ├── libnixmain.dylib -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/lib/libnixmain.dylib
│   ├── libnixstore.dylib -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/lib/libnixstore.dylib
│   ├── libnixutil.dylib -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/lib/libnixutil.dylib
│   ├── links -> /nix/store/azgb01k93yxk91h82cxm6i9mk4z7xwaj-fast-tags-1.3/lib/links
│   ├── perl5 -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/lib/perl5
│   ├── pkgconfig
│   │   ├── gail-3.0.pc -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/lib/pkgconfig/gail-3.0.pc
│   │   ├── gdk-3.0.pc -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/lib/pkgconfig/gdk-3.0.pc
│   │   ├── gdk-quartz-3.0.pc -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/lib/pkgconfig/gdk-quartz-3.0.pc
│   │   ├── gtk+-3.0.pc -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/lib/pkgconfig/gtk+-3.0.pc
│   │   ├── gtk+-quartz-3.0.pc -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/lib/pkgconfig/gtk+-quartz-3.0.pc
│   │   ├── gtk+-unix-print-3.0.pc -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/lib/pkgconfig/gtk+-unix-print-3.0.pc
│   │   ├── nix-expr.pc -> /nix/store/aqjvzzzkfl000kc2b099z3skkynsvf1f-nix-1.11.6-dev/lib/pkgconfig/nix-expr.pc
│   │   ├── nix-main.pc -> /nix/store/aqjvzzzkfl000kc2b099z3skkynsvf1f-nix-1.11.6-dev/lib/pkgconfig/nix-main.pc
│   │   └── nix-store.pc -> /nix/store/aqjvzzzkfl000kc2b099z3skkynsvf1f-nix-1.11.6-dev/lib/pkgconfig/nix-store.pc
│   └── python2.7 -> /nix/store/hsy0kwjp04rzy4bbajxkb967qppql618-nixops-1.4/lib/python2.7
├── libexec
│   ├── cabal2nix-2.0.4 -> /nix/store/njarvl56b0h8fwz30ifizadiyh3gmj7q-cabal2nix-2.0.4/libexec/cabal2nix-2.0.4
│   └── nix -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/libexec/nix
├── manifest.nix -> /nix/store/60in7ak6v7sckgv7l460g8v93hnj7yvv-env-manifest.nix
└── share
    ├── aclocal -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/aclocal
    ├── bash-completion -> /nix/store/njarvl56b0h8fwz30ifizadiyh3gmj7q-cabal2nix-2.0.4/share/bash-completion
    ├── doc
    │   ├── nix -> /nix/store/pbghw9nqn4203kaxm75pj2dhi682d3mz-nix-1.11.6-doc/share/doc/nix
    │   ├── nixops -> /nix/store/hsy0kwjp04rzy4bbajxkb967qppql618-nixops-1.4/share/doc/nixops
    │   ├── x86_64-osx-ghc-8.0.1
    │   │   ├── ShellCheck-0.4.4 -> /nix/store/sg2pnmp4kg4fj7s5vrk0jmbcxsv0cbrf-ShellCheck-0.4.4/share/doc/x86_64-osx-ghc-8.0.1/ShellCheck-0.4.4
    │   │   ├── cabal-install-1.24.0.0 -> /nix/store/a41vdrzivdqi7hn9fslhhmfsrxxlcd7r-cabal-install-1.24.0.0/share/doc/x86_64-osx-ghc-8.0.1/cabal-install-1.24.0.0
    │   │   ├── delegate-1.0.0 -> /nix/store/wbi1jydszj8vx45jvfwlf6phy3577vzz-delegate-1.0.0/share/doc/x86_64-osx-ghc-8.0.1/delegate-1.0.0
    │   │   ├── dhall-1.1.0 -> /nix/store/xk0ddz8f9f74db6a5bvj38bn5gz4l5gx-dhall-1.0.2/share/doc/x86_64-osx-ghc-8.0.1/dhall-1.1.0
    │   │   └── grpc-haskell-0.0.0.0 -> /nix/store/si7h1b72646yw8rqc1dn0q2kz61x26wa-grpc-haskell-0.0.0.0/share/doc/x86_64-osx-ghc-8.0.1/grpc-haskell-0.0.0.0
    │   └── x86_64-osx-ghc-8.0.2 -> /nix/store/azgb01k93yxk91h82cxm6i9mk4z7xwaj-fast-tags-1.3/share/doc/x86_64-osx-ghc-8.0.2
    ├── emacs -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/share/emacs
    ├── gettext -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/share/gettext
    ├── gir-1.0 -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/gir-1.0
    ├── go -> /nix/store/aprz5k01f5fy9lmcxqgh42q4d4lfzq4s-go2nix-1.1.1/share/go
    ├── gsettings-schemas -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/share/gsettings-schemas
    ├── gtk-3.0 -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/share/gtk-3.0
    ├── locale -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/share/locale
    ├── man
    │   ├── man1
    │   │   ├── broadwayd.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/broadwayd.1.gz
    │   │   ├── cabal.1.gz -> /nix/store/a41vdrzivdqi7hn9fslhhmfsrxxlcd7r-cabal-install-1.24.0.0/share/man/man1/cabal.1.gz
    │   │   ├── gtk-builder-tool.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk-builder-tool.1.gz
    │   │   ├── gtk-encode-symbolic-svg.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk-encode-symbolic-svg.1.gz
    │   │   ├── gtk-launch.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk-launch.1.gz
    │   │   ├── gtk-query-immodules-3.0.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk-query-immodules-3.0.1.gz
    │   │   ├── gtk-query-settings.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk-query-settings.1.gz
    │   │   ├── gtk-update-icon-cache.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk-update-icon-cache.1.gz
    │   │   ├── gtk3-demo-application.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk3-demo-application.1.gz
    │   │   ├── gtk3-demo.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk3-demo.1.gz
    │   │   ├── gtk3-icon-browser.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk3-icon-browser.1.gz
    │   │   ├── gtk3-widget-factory.1.gz -> /nix/store/g5mslinp00c9901gigfv4sshc1frck6k-gtk+3-3.22.12-dev/share/man/man1/gtk3-widget-factory.1.gz
    │   │   ├── nix-build.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-build.1.gz
    │   │   ├── nix-channel.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-channel.1.gz
    │   │   ├── nix-collect-garbage.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-collect-garbage.1.gz
    │   │   ├── nix-copy-closure.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-copy-closure.1.gz
    │   │   ├── nix-env.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-env.1.gz
    │   │   ├── nix-hash.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-hash.1.gz
    │   │   ├── nix-install-package.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-install-package.1.gz
    │   │   ├── nix-instantiate.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-instantiate.1.gz
    │   │   ├── nix-prefetch-url.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-prefetch-url.1.gz
    │   │   ├── nix-pull.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-pull.1.gz
    │   │   ├── nix-push.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-push.1.gz
    │   │   ├── nix-shell.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-shell.1.gz
    │   │   ├── nix-store.1.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man1/nix-store.1.gz
    │   │   ├── nixops.1.gz -> /nix/store/hsy0kwjp04rzy4bbajxkb967qppql618-nixops-1.4/share/man/man1/nixops.1.gz
    │   │   ├── rsync.1.gz -> /nix/store/1qcnb87mrk2m8d0wpx9m9awdq8ryyckb-rsync-3.1.2/share/man/man1/rsync.1.gz
    │   │   └── tree.1.gz -> /nix/store/04y5lqwn6967yni50nx1vhsmjdcwm12z-tree-1.7.0/share/man/man1/tree.1.gz
    │   ├── man5
    │   │   ├── nix.conf.5.gz -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man5/nix.conf.5.gz
    │   │   └── rsyncd.conf.5.gz -> /nix/store/1qcnb87mrk2m8d0wpx9m9awdq8ryyckb-rsync-3.1.2/share/man/man5/rsyncd.conf.5.gz
    │   └── man8 -> /nix/store/pmfhnmvmfxki5r02sqg7k1xpbk74s8wi-nix-1.11.6-man/share/man/man8
    ├── nix
    │   ├── corepkgs -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/share/nix/corepkgs
    │   ├── nixops -> /nix/store/hsy0kwjp04rzy4bbajxkb967qppql618-nixops-1.4/share/nix/nixops
    │   └── sandbox-defaults.sb -> /nix/store/awyp4bxj9xnyxxwaig861q3b1gb7jhzh-nix-1.11.6/share/nix/sandbox-defaults.sb
    ├── themes -> /nix/store/dw7vlwmfw0m46jpa9p6204niicl9k591-gtk+3-3.22.12/share/themes
    └── x86_64-osx-ghc-8.0.2 -> /nix/store/azgb01k93yxk91h82cxm6i9mk4z7xwaj-fast-tags-1.3/share/x86_64-osx-ghc-8.0.2

60 directories, 109 files
```

# Profiles - `brew`

`brew` stores all packages underneath `/usr/local`

Like Nix, these files are symlinks into Brew's own file store:

```bash
$ readlink /usr/local/bin/gtk-demo 
../Cellar/gtk+/2.24.30/bin/gtk-demo
```

# Rollbacks

With Nix you can roll back any change to your environment:

```bash
$ nix-env --install hello --file '<nixpkgs>'
...
building path(s) ‘/nix/store/fyhxvhprpzfp48w8yrgrb717lamqqss1-user-environment’
created 171 symlinks in user environment
$ hello
Hello, world!

$ nix-env --uninstall hello
$ hello
-bash: hello: command not found

$ nix-env --rollback  # Yes, you can rollback *uninstalling* a package
switching from generation 433 to 432
$ hello
Hello, world!
```

This is because Nix stores immutable snapshots of your development environment

These snapshots are called "generations"

# Listing generations

```bash
$ nix-env --list-generations
 ...
 452   2017-05-23 21:21:34   
 453   2017-05-23 21:22:44   
 454   2017-05-23 21:24:11   
 455   2017-05-23 21:34:12   
 456   2017-05-23 21:36:29   
 457   2017-05-23 21:38:50   
 458   2017-05-23 21:42:30   
 459   2017-05-23 21:55:40   
 460   2017-05-23 21:57:56   
 461   2017-05-23 22:20:13   (current)
```

These generations are located under `/nix/var/nix/profiles`:

```bash
$ ls /nix/var/nix/profiles/
default			default-443-link	default-454-link
default-433-link	default-444-link	default-455-link
default-434-link	default-445-link	default-456-link
default-435-link	default-446-link	default-457-link
default-436-link	default-447-link	default-458-link
default-437-link	default-448-link	default-459-link
default-438-link	default-449-link	default-460-link
default-439-link	default-450-link	default-461-link
default-440-link	default-451-link	per-user
default-441-link	default-452-link
default-442-link	default-453-link
```

# Uninstallation

Even when we "uninstall" a package it is still in the `/nix/store`:

```bash
$ nix-store --query --requisites ~/.nix-profile | grep hello
/nix/store/h5paliil3r6m70na37ymba1f007mm28k-hello-2.10
$ nix-env --uninstall hello
uninstalling ‘hello-2.10’
$ nix-store --query --requisites ~/.nix-profile | grep hello
$ du -hs /nix/store/h5paliil3r6m70na37ymba1f007mm28k-hello-2.10
 76K	/nix/store/h5paliil3r6m70na37ymba1f007mm28k-hello-2.10
```

... and Nix won't let us manually delete the package from the store:

```bash
$ nix-store --delete /nix/store/h5paliil3r6m70na37ymba1f007mm28k-hello-2.10
finding garbage collector roots...
0 store paths deleted, 0.00 MiB freed
error: cannot delete path ‘/nix/store/h5paliil3r6m70na37ymba1f007mm28k-hello-2.10’ since it is still alive
```

... because an older version of our development environment depends on it

# Garbage collection roots

Nix removes unused packages via garbage collection, which you can manually run:

```bash
$ nix-collect-garbage  # This command is always safe to run
```

... but garbage collection alone won't uninstall packages

You have first to delete older generations of your development environment

You can query the GC root associated with a package using:

```bash
$ nix-store --query --roots !$
nix-store --query --roots /nix/store/h5paliil3r6m70na37ymba1f007mm28k-hello-2.10
/nix/var/nix/profiles/default-432-link
```

# Removing old generations

You can delete and garbage collect specific generations:

```bash
$ nix-env --delete-generations 431 432
$ nix-collect-garbage
```

... or you can delete and garbage collect all generations older than some time:

```bash
$ nix-collect-garbage --delete-older-than 30d
```

... or you can delete and garbage collect all older generations:

```bash
$ nix-collect-garbage --delete
```

**CAREFUL**: You can't roll back to a deleted generation

# GC-related commands

The following commands are very handy when debugging Nix and garbage collection:

```bash
$ # Find all garbage collection roots that depend on a given path
$ nix-store --query --roots /nix/store/...

$ # List all garbage collection roots
$ nix-store --gc --print-roots

$ # List all dependencies of a path
$ nix-store --query --requisites /nix/store/...

$ # Pretty-print all dependencies of a path as a tree
$ nix-store --query --requisites /nix/store/...
```

Also, the above commands work with files that are symlinks to `/nix/store`, like:

* `~/.nix-profile` (your current development profile)
* `/run/current-system` (NixOS system configuration)

# Channels

Channels are one way to distribute Nix source and pre-built artifacts

A channel corresponds roughly to a software distribution

Analogous to Debian's stable, testing, and unstable distributions

Also analogous to `brew`'s taps

# Subscribing to a channel

`nix-channel`:

```bash
$ nix-channel --add https://nixos.org/channels/nixos-17.03-small nixpkgs
$ nix-channel --update nixpkgs
```

`brew`:

```bash
$ brew tap homebrew/science
```

# Updating a channel

`nix-channel`:

```bash
$ nix-channel --update nixpkgs
$ nix-env --upgrade --file '<nixpkgs>'
```

`brew`:

```bash
$ brew update
```

# Unsubscribe from a channel

`nix-channel`:

```bash
$ nix-channel --remove nixpkgs
```

`brew`:

```bash
$ brew untap homebrew/science
```

# Rollback channel changes

`nix-channel`:

```bash
$ nix-channel --rollback
```

There is no analog of this for `brew`

# Questions?

* How to author a minimal Nix package
* Basic package management
* **Development environments**
* Our internal use of Nix packages
* Conclusions

# Development environment

The `nix-shell` command lets you create *transient* and *isolated* development
environment

For example, if I want to use the `go` compiler without installing it:

```bash
Gabriels-MacBook-Pro:end-to-end gabriel$ nix-shell --packages go
[nix-shell:~/proj/end-to-end]$ go --help
Go is a tool for managing Go source code.

Usage:

	go command [arguments]

...
[nix-shell:~/proj/end-to-end]$ exit
Gabriels-MacBook-Pro:end-to-end gabriel$ go --help
-bash: go: command not found
```

# Packages

`nix-shell` can manage installed libraries, too:

```bash
$ nix-shell --packages 'haskellPackages.ghcWithPackages (pkgs: [ pkgs.lens pkgs.lens-aeson ])'
$ ghci
>>> import Control.Lens
>>> import Data.Aeson.Lens
>>> over (key "foo" . values . _Integer) negate "{\"foo\":[1,2,3],\"bar\":[4,5,6]}"
"{\"foo\":[-1,-2,-3],\"bar\":[4,5,6]}"
```

The method for doing this varies depending on the language ecosystem

The [`nixpkgs` manual](http://nixos.org/nixpkgs/manual/) contains instructions
for each programming language ecosystem

# `myEnvFun`

For more sophisticated environments you can use the `myEnvFun` utility:

```haskell
let
  pkgs = import <nixpkgs> { };

in
  pkgs.myEnvFun {
    name = "foo";

    buildInputs = [
      pkgs.cabal-install
      pkgs.go
      pkgs.sbt
    ];
  }
```

```bash
$ nix-env --install --file dev.nix
$ load-env-foo  # Now you have a transient environment
```

# `awake-webservices`

Our `awake-pkgs` repository provides several per-project development environments

For example, here is how I can set up a development environment for `awake-scala`:

```bash
$ nix-shell -A awake-scala.devenv release.nix 
[nix-shell:~/proj/awake-pkgs/.project-repos/awake-scala-devenv.git]$ exit
```

# Questions?

* How to author a minimal Nix package
* Basic package management
* Development environments
* **Our internal use of Nix packages**
* Conclusions

# `NIX_PATH`

We've seen `<nixpkgs>` in several previous examples:

```bash
$ nix-env --install hello --file '<nixpkgs>'
...
```

```haskell
let
  pkgs = import <nixpkgs> { };

in
  ...
```

Anything in angle brackets is a reference to the `NIX_PATH` environment variable

```bash
$ echo "${NIX_PATH}"
nixpkgs=/nix/store/qk1q2rwq9qzhi49hx7whji90bqk8kf9y-nixpkgs-7ae9da426924537755ce9164fd5b5f81ce16a1c3-src:nixpkgs=/Users/gabriel/.nix-defexpr/channels/nixpkgs
```

# Pinning the `NIX_PATH`

The `./setup/init.sh` in `end-to-end` pins our `NIX_PATH` so that we are all
using the same version of `nixpkgs`

Pros:

* Everybody in the company using the exact same version
* We can ensure that key software we depend on is prebuilt and cached

Cons:

* Things we don't prebuild usually cache miss since we're behind public cache

You can work around this by doing:

```bash
$ nix-env --install hello --file https://github.com/NixOS/nixpkgs/archive/master.tar.gz
```

There is no `brew` analog of pinning the package repository

You have to use whatever is the latest version available

# Hydra

We build and cache artifacts using Hydra, which you can find here:

* [http://hydra.mv.awakenetworks.net/](http://hydra.mv.awakenetworks.net/)

DEMO: Browse Hydra

I will cover Hydra in more detail in a later brownbag comparing Hydra to Jenkins

# Installing tools from Hydra

Hydra serves packages that you can install from the command line:

```bash
nix-install-package --non-interactive --url http://hydra.mv.awakenetworks.net:3000/job/awake-pkgs/master/delegate/latest/nix/pkg/delegate-1.0.0-x86_64-darwin.nixpkg
```

You can also install from the `awake-pkgs` repository using `nix-env`:

```bash
$ nix-env --install --attr awake-webservices --file release.nix 
...
$ awake-webservices.git --help
NAME:
   awake-webservices.git - Web services for awake

USAGE:
   awake-webservices.git [global options] command [command options] [arguments...]
   
...
```

# Adding new builds to Hydra

We use the convention that each repository has a `release.nix` file that
Hydra builds

The most widely used one is the `release.nix` file in `end-to-end`

Hydra automatically builds anything in the `release.nix` file of `end-to-end`

If you want to build and cache new things, just make a PR to `end-to-end`

DEMO: Pull request to build `sbt`

# Creating channels

Hydra automatically generates channels

Example: [`awake-pkgs`](http://hydra.mv.awakenetworks.net/jobset/awake-pkgs/master/channel/latest)

However, this won't work out of the box for OS X!

# Questions?

* How to author a minimal Nix package
* Basic package management
* Development environments
* Our internal use of Nix packages
* **Conclusions**

# Conclusions

The better you get with Nix the less you install tools on your global path

You can usually use `nix-build` and `nix-shell` to do everything

Read the [Nix manual](http://nixos.org/nix/manual/) if you want to learn more
