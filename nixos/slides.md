% NixOS administration
% Gabriel Gonzalez
% July 11, 2018

# Motivation

This talk covers how to administer NixOS effectively in production

This talk discusses both how things should be done ideally and how we actually
do things (i.e. technical debt)

# NixOS: desktop vs production

NixOS provides feature for use both as a desktop OS and a production OS

**Desktop OS workflow**: "Edit" the system via command line

* Traditional Linux system administration
* `nix-env` + `nix-channel` + `nixos-rebuild`

**Production OS workflow**: Declare what system should be via source code:

* Modern `docker`-style system administration
* `$EDITOR` + `nix-build` + `result/bin/switch-to-configuration`

We started with desktop workflow, but we're transitioning to production workflow

This talk mostly focuses on the production OS workflow

# Nix basics

Everything built by Nix goes through the following pipeline:

```
Expression → Derivation → Output
```

* **Expression:** An expression in the Nix programming language

  Nix is a general purpose functional programming language

  Note: You can evaluate expressions that do not represent things to build

* **Derivation:** A `/nix/store` file ending in `.drv`

  Language-independent instructions for how to build things

  Note: The derivation format is completely Nix-independent

* **Output:** A `/nix/store/` file or directory not ending in `.drv`

  Something built by Nix

  Note: By default Nix commands create a `./result` symlink to the output

# Command line - Building a software package

Each step in the pipeline corresponds to a Nix command:

```
Expression →(nix-instantiate)→ Derivation →(nix-store --realise)→ Output
```

```
$ cat test.nix
let
  pkgs = import <nixpkgs> { };

in
  pkgs.hello
```

```
$ nix-instantiate --eval test.nix
{ __ignoreNulls = true; __impureHostDeps = <CODE>; __propagatedImpureHostDeps =
<CODE>; __propagatedSandboxProfile = <CODE>; __sandboxProfile = <CODE>; all = <C
ODE>; args = <CODE>; buildInputs = <CODE>; builder = <CODE>; configureFlags = <C
ODE>; doCheck = true; drvAttrs = { __ignoreNulls = true; __impureHostDeps = <COD
E>; __propagatedImpureHostDeps = <CODE>; __propagatedSandboxProfile = <CODE>; __
sandboxProfile = <CODE>; args = <CODE>; buildInputs = <CODE>; builder = <CODE>;
configureFlags = <CODE>; doCheck = true; name = <CODE>; nativeBuildInputs = <COD
E>; propagatedBuildInputs = <CODE>; propagatedNativeBuildInputs = <CODE>; src =
<CODE>; stdenv = { __extraImpureHostDeps = <CODE>; __impureHostDeps = <CODE>; __
sandboxProfile = <CODE>; all = <CODE>; allowedRequisites = <CODE>; args = <CODE>
...
x86 = true; isx86_64 = true; libc = "libSystem"; parsed = { _type = "system"; ab
i = { _type = "abi"; name = "unknown"; }; cpu = { _type = "cpu-type"; bits = 64;
 family = "x86"; name = "x86_64"; significantByte = { _type = "significant-byte"
; name = "littleEndian"; }; }; kernel = { _type = "kernel"; execFormat = { _type
 = "exec-format"; name = "macho"; }; families = { }; name = "darwin"; }; vendor
= { _type = "vendor"; name = "apple"; }; }; platform = { kernelAutoModules = tru
e; kernelBaseConfig = "defconfig"; kernelHeadersBaseConfig = "defconfig"; kernel
Target = "bzImage"; name = "pc"; uboot = null; }; system = "x86_64-darwin"; }; t
ype = "derivation"; }; system = <CODE>; type = "derivation"; userHook = <CODE>;
}
```

```
$ nix-instantiate test.nix
warning: you did not specify ‘--add-root’; the result might be removed by the garbage collector
/nix/store/3mi0vs2bajrvwa2vn9szbkgkhigz9xwa-hello-2.10.drv
```

```
$ fold /nix/store/3mi0vs2bajrvwa2vn9szbkgkhigz9xwa-hello-2.10.drv
Derive([("out","/nix/store/as96dxdmihr1xfligz5m89ablwrhy057-hello-2.10","","")],
[("/nix/store/35pmyvq35kjd4rs188rcg7nkl97d091p-bash-4.4-p12.drv",["out"]),("/nix
/store/f2l1rp541dmv6gvqmv4r19x5z10596br-stdenv-darwin.drv",["out"]),("/nix/store
/p82824229m0afw42zb653i1nzhj1w6xr-hello-2.10.tar.gz.drv",["out"])],["/nix/store/
9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh","/nix/store/z347hsajryw593h
802ggb63lbr3gpv2b-standard-sandbox.sb"],"x86_64-darwin","/nix/store/03pdhzb1qlgn
4jil6kbxy75dcqpd1m1a-bash-4.4-p12/bin/bash",["-e","/nix/store/9krlzvny65gdc8s7kp
b6lkx8cd02c25b-default-builder.sh"],[("__impureHostDeps","/System/Library/Framew
orks/CoreFoundation.framework/CoreFoundation /dev/zero /dev/random /dev/urandom
/bin/sh"),("__propagatedImpureHostDeps",""),("__propagatedSandboxProfile",""),("
__sandboxProfile","(allow file-read* (literal \"/usr/lib/libncurses.5.4.dylib\")
)\n(import \"/nix/store/z347hsajryw593h802ggb63lbr3gpv2b-standard-sandbox.sb\")\
n"),("buildInputs",""),("builder","/nix/store/03pdhzb1qlgn4jil6kbxy75dcqpd1m1a-b
ash-4.4-p12/bin/bash"),("configureFlags",""),("doCheck","1"),("name","hello-2.10
"),("nativeBuildInputs",""),("out","/nix/store/as96dxdmihr1xfligz5m89ablwrhy057-
hello-2.10"),("propagatedBuildInputs",""),("propagatedNativeBuildInputs",""),("s
rc","/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz"),("stdenv","
/nix/store/bh2q1fwjk7mv2rjq9b5gkd564lzsdrql-stdenv-darwin"),("system","x86_64-da
rwin")])
```

```
$ nix-store --realise /nix/store/3mi0vs2bajrvwa2vn9szbkgkhigz9xwa-hello-2.10.drv
these paths will be fetched (0.02 MiB download, 0.07 MiB unpacked):
  /nix/store/as96dxdmihr1xfligz5m89ablwrhy057-hello-2.10
fetching path ‘/nix/store/as96dxdmihr1xfligz5m89ablwrhy057-hello-2.10’...

*** Downloading ‘https://cache.nixos.org/nar/1mkfcfhd8xml3qbs54b3app9yz65b9m01ggd00lc556vsbdvsy3a.nar.xz’ (signed by ‘cache.nixos.org-1’) to ‘/nix/store/as96dxdmihr1xfligz5m89ablwrhy057-hello-2.10’...
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 23300  100 23300    0     0  23300      0  0:00:01 --:--:--  0:00:01 35901

warning: you did not specify ‘--add-root’; the result might be removed by the garbage collector
/nix/store/as96dxdmihr1xfligz5m89ablwrhy057-hello-2.10
```

```
$ /nix/store/as96dxdmihr1xfligz5m89ablwrhy057-hello-2.10/bin/hello
Hello, world!
```

```
$ ./result/bin/hello
Hello, world!
```

# `nix-build`

`nix-build` is the same as `nix-instantiate` + `nix-store --realise`

```bash
$ nix-instantiate test.nix
...
/nix/store/3mi0vs2bajrvwa2vn9szbkgkhigz9xwa-hello-2.10.drv

$ nix-store --realise /nix/store/3mi0vs2bajrvwa2vn9szbkgkhigz9xwa-hello-2.10.drv
...
/nix/store/as96dxdmihr1xfligz5m89ablwrhy057-hello-2.10
```

```bash
$ nix-build test.nix
/nix/store/as96dxdmihr1xfligz5m89ablwrhy057-hello-2.10
```

# NixOS

Here is what the pipeline looks like with NixOS:

```
Expression (NixOS configuration) → Derivation → Output → Deployed system
```

A NixOS configuration is just a (very large) Nix expression

Instantiating a NixOS configuration still produces a derivation

The output is a directory tree containing a "ready-to-deploy" Linux system

```
Output →(sudo ./result/bin/switch-to-configuration switch)→ Deployed system
```

# Command line - Deploying to the current machine

```
$ ssh hydra
$ cd /path/to/checkout/of/end-to-end
```

```
$ nix-instantiate --attr system-hydra-master release.nix
warning: you did not specify ‘--add-root’; the result might be removed by the garbage collector
/nix/store/l74sry6z2k86sjjqyl1bxln8ahw78536-nixos-system-hydra-17.09pre-git.drv
```

```
$ nix-store --realise /nix/store/l74sry6z2k86sjjqyl1bxln8ahw78536-nixos-system-hydra-17.09pre-git.drv
warning: you did not specify ‘--add-root’; the result might be removed by the garbage collector
/nix/store/pmpd4mnk0mb74lw6n47n2hhbjp8qm02b-nixos-system-hydra-17.09pre-git
```

```
$ # Same as: sudo ./result/bin/switch-to-configuration switch
$ sudo /nix/store/pmpd4mnk0mb74lw6n47n2hhbjp8qm02b-nixos-system-hydra-17.09pre-git/bin/switch-to-configuration switch
updating GRUB 2 menu...
stopping the following units: shipit.service
activating the configuration...
warning: not applying UID change of user ‘hydra-queue-runner’ (1007 -> 235)
setting up /etc...
setting up tmpfiles
starting the following units: shipit.service
```

# Command line - Deploying to a remote machine

```
$ # On my own machine
$ cd /path/to/checkout/of/end-to-end
```

```
$ nix-instantiate --attr system-hydra-master release.nix
warning: you did not specify ‘--add-root’; the result might be removed by the garbage collector
/nix/store/l74sry6z2k86sjjqyl1bxln8ahw78536-nixos-system-hydra-17.09pre-git.drv
```

```
$ nix-store --realise /nix/store/l74sry6z2k86sjjqyl1bxln8ahw78536-nixos-system-hydra-17.09pre-git.drv
warning: you did not specify ‘--add-root’; the result might be removed by the garbage collector
/nix/store/pmpd4mnk0mb74lw6n47n2hhbjp8qm02b-nixos-system-hydra-17.09pre-git
```

```
$ nix-copy-closure --to hydra /nix/store/pmpd4mnk0mb74lw6n47n2hhbjp8qm02b-nixos-system-hydra-17.09pre-git
```

```
$ ssh hydra sudo /nix/store/pmpd4mnk0mb74lw6n47n2hhbjp8qm02b-nixos-system-hydra-17.09pre-git/bin/switch-to-configuration switch
updating GRUB 2 menu...
stopping the following units: shipit.service
activating the configuration...
warning: not applying UID change of user ‘hydra-queue-runner’ (1007 -> 235)
setting up /etc...
setting up tmpfiles
starting the following units: shipit.service
```

# Minimal NixOS system

```nix
let
  nixos = import <nixpkgs/nixos> {
    system = "x86_64-linux";

    configuration = {
      boot.loader.grub.devices = [ "/dev/sda" ];

      fileSystems."/".device = "/dev/disk/by-label/root";

      services.openssh.enable = true;

      users = {
        mutableUsers = false;

        users.gabriel = {
          isNormalUser = true;

          extraGroups = [ "wheel" ];

          openssh.authorizedKeys.keys =
            [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICz3QpMYr+cDw4XcZ1di09pT7FTN/pa01RcjJLjyIg/+ example@example.com" ];
        };
      };
    };
  };

in
  nixos.system
```

```bash
$ nix-build test.nix
$ sudo result/bin/switch-to-configuration boot
```

# System distributions

Our system definition is small because most of the logic lives in `nixpkgs`

`nixpkgs` is a large monorepo on GitHub repository containing a lot of Nix code

You can think of `nixpkgs` revisions as analogous to distribution releases

`nixpkgs` has stable branches named `release-MM.NN` (i.e. `release-18.09`):

* These would be analogous to Debian stable or oldstable releases

`nixpkgs` also has a `nixpkgs-unstable` branch

* This would be analogous to Debian unstable

`nixpkgs` `master` is bleeding edge

# Specifying the `nixpkgs` release

We can pin to a specific revision of `nixpkgs` like this:

```nix
let
  # https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
     rev          = "74286ec9e76be7cd00c4247b9acb430c4bd9f1ce";
     sha256       = "0njb3qd2wxj7gil8y61lwh7zacmvr6zklv67w5zmvifi1fvalvdg";
     outputSha256 = "13ydgpzl5nix4gc358iy9zjd5nrrpbpwpxmfhis4aai2zmkja3ak";
  };
  
  nixos = import "${nixpkgs}/nixos" {
    system = "x86_64-linux";

    configuration = {
      boot.loader.grub.devices = [ "/dev/sda" ];

      fileSystems."/".device = "/dev/disk/by-label/root";

      services.openssh.enable = true;

      users = {
        mutableUsers = false;

        users.gabriel = {
          isNormalUser = true;

          extraGroups = [ "wheel" ];

          openssh.authorizedKeys.keys =
            [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICz3QpMYr+cDw4XcZ1di09pT7FTN/pa01RcjJLjyIg/+ example@example.com" ];
        };
      };
    };
  };

in
  nixos.system
```

# Upgrading the system

The way we "upgrade" our system is to specify a more recent revision of
`nixpkgs`

We usually pick the latest revision of the latest stable release when we upgrade

```nix
  # This is the part you modify to upgrade
  nixpkgs = fetchNixpkgs {
     rev          = "74286ec9e76be7cd00c4247b9acb430c4bd9f1ce";
     sha256       = "0njb3qd2wxj7gil8y61lwh7zacmvr6zklv67w5zmvifi1fvalvdg";
     outputSha256 = "13ydgpzl5nix4gc358iy9zjd5nrrpbpwpxmfhis4aai2zmkja3ak";
  };
```

Usually we change the `rev` field and try to build it, fixing the `sha256`
and `outputSha256` fields in response to error messages

This logic currently lives in `awake-pkgs/nixpkgs/17_09.nix` in our repository

# Browsing available software (Web)

You can use your browser to crudely browse available packages using this URL:

```
https://raw.githubusercontent.com/NixOS/nixpkgs/${REVISION}/pkgs/top-level/all-packages.nix
```

For example:

[https://raw.githubusercontent.com/NixOS/nixpkgs/74286ec9e76be7cd00c4247b9acb430c4bd9f1ce/pkgs/top-level/python-packages.nix](https://raw.githubusercontent.com/NixOS/nixpkgs/74286ec9e76be7cd00c4247b9acb430c4bd9f1ce/pkgs/top-level/python-packages.nix)

That page lists most useful packages that you can install

# Browing available software (REPL)

You can more systematically explore available packages using `nix-repl`:

```
nix-repl> fetchNixpkgs = import ./fetchNixpkgs.nix

nix-repl> nixpkgs = fetchNixpkgs { rev = "74286ec9e76be7cd00c4247b9acb430c4bd9f1ce"; sha256 = "0njb3qd2wxj7gil8y61lwh7zacmvr6zklv67w5zmvifi1fvalvdg"; outputSha256 = "13ydgpzl5nix4gc358iy9zjd5nrrpbpwpxmfhis4aai2zmkja3ak"; }

nix-repl> pkgs = import nixpkgs { config = {}; }

nix-repl> pkgs.<TAB>
Display all 7887 possibilities? (y or n)
pkgs.2048-in-terminal
pkgs.2bwm
pkgs.389-ds-base
pkgs.90secondportraits
pkgs.AAAAAASomeThingsFailToEvaluate
pkgs.SDL2
...

nix-repl> pkgs.python<TAB>
pkgs.python             pkgs.python3            pkgs.python36Packages
pkgs.python-setup-hook  pkgs.python34           pkgs.python3Full
pkgs.python2            pkgs.python34Full       pkgs.python3Packages
pkgs.python27           pkgs.python34Packages   pkgs.pythonDocs
pkgs.python27Full       pkgs.python35           pkgs.pythonFull
pkgs.python27Packages   pkgs.python35Full       pkgs.pythonIRClib
pkgs.python2Full        pkgs.python35Packages   pkgs.pythonPackages
pkgs.python2Packages    pkgs.python36           pkgs.pythonSexy
pkgs.python2nix         pkgs.python36Full       pkgs.pythonmagick

nix-repl> pkgs.pythonPackages.requests<TAB>
pkgs.pythonPackages.requests
pkgs.pythonPackages.requests-cache
pkgs.pythonPackages.requests-mock
pkgs.pythonPackages.requests2
pkgs.pythonPackages.requests_download
pkgs.pythonPackages.requests_ntlm
pkgs.pythonPackages.requests_oauth2
pkgs.pythonPackages.requests_oauthlib
pkgs.pythonPackages.requests_toolbelt
pkgs.pythonPackages.requestsexceptions

nix-repl> pkgs.haskell.compiler.<TAB>
pkgs.haskell.compiler.ghc6102Binary   pkgs.haskell.compiler.ghc784
pkgs.haskell.compiler.ghc6104         pkgs.haskell.compiler.ghc801
pkgs.haskell.compiler.ghc6123         pkgs.haskell.compiler.ghc802
pkgs.haskell.compiler.ghc704          pkgs.haskell.compiler.ghc821
pkgs.haskell.compiler.ghc704Binary    pkgs.haskell.compiler.ghc822
pkgs.haskell.compiler.ghc7102         pkgs.haskell.compiler.ghcHEAD
pkgs.haskell.compiler.ghc7103         pkgs.haskell.compiler.ghcHaLVM240
pkgs.haskell.compiler.ghc722          pkgs.haskell.compiler.ghcjs
pkgs.haskell.compiler.ghc742          pkgs.haskell.compiler.ghcjsHEAD
pkgs.haskell.compiler.ghc742Binary    pkgs.haskell.compiler.integer-simple
pkgs.haskell.compiler.ghc763          pkgs.haskell.compiler.jhc
pkgs.haskell.compiler.ghc783          pkgs.haskell.compiler.uhc

nix-repl> pkgs.haskellPackages.dhall<TAB>
pkgs.haskellPackages.dhall        pkgs.haskellPackages.dhall-json
pkgs.haskellPackages.dhall-bash   pkgs.haskellPackages.dhall-nix
pkgs.haskellPackages.dhall-check  pkgs.haskellPackages.dhall-text

nix-repl> pkgs.haskellPackages.dhall.<TAB>
pkgs.haskellPackages.dhall.LANG
pkgs.haskellPackages.dhall.__ignoreNulls
pkgs.haskellPackages.dhall.__impureHostDeps
pkgs.haskellPackages.dhall.__propagatedImpureHostDeps
pkgs.haskellPackages.dhall.__propagatedSandboxProfile
pkgs.haskellPackages.dhall.__sandboxProfile
pkgs.haskellPackages.dhall.all
pkgs.haskellPackages.dhall.args
pkgs.haskellPackages.dhall.buildInputs
pkgs.haskellPackages.dhall.buildPhase
pkgs.haskellPackages.dhall.builder
pkgs.haskellPackages.dhall.checkPhase
pkgs.haskellPackages.dhall.compileBuildDriverPhase
pkgs.haskellPackages.dhall.configureFlags
pkgs.haskellPackages.dhall.configurePhase
pkgs.haskellPackages.dhall.doCheck
pkgs.haskellPackages.dhall.doc
pkgs.haskellPackages.dhall.drvAttrs
pkgs.haskellPackages.dhall.drvPath
pkgs.haskellPackages.dhall.env
pkgs.haskellPackages.dhall.haddockDir
pkgs.haskellPackages.dhall.haddockPhase
pkgs.haskellPackages.dhall.installPhase
pkgs.haskellPackages.dhall.isHaskellLibrary
pkgs.haskellPackages.dhall.meta
pkgs.haskellPackages.dhall.name
pkgs.haskellPackages.dhall.nativeBuildInputs
pkgs.haskellPackages.dhall.out
pkgs.haskellPackages.dhall.outPath
pkgs.haskellPackages.dhall.outputName
pkgs.haskellPackages.dhall.outputUnspecified
pkgs.haskellPackages.dhall.outputs
pkgs.haskellPackages.dhall.override
pkgs.haskellPackages.dhall.overrideAttrs
pkgs.haskellPackages.dhall.overrideDerivation
pkgs.haskellPackages.dhall.overrideScope
pkgs.haskellPackages.dhall.passthru
pkgs.haskellPackages.dhall.pname
pkgs.haskellPackages.dhall.postPatch
pkgs.haskellPackages.dhall.preConfigurePhases
pkgs.haskellPackages.dhall.preInstallPhases
pkgs.haskellPackages.dhall.prePatch
pkgs.haskellPackages.dhall.prePhases
pkgs.haskellPackages.dhall.propagatedBuildInputs
pkgs.haskellPackages.dhall.propagatedNativeBuildInputs
pkgs.haskellPackages.dhall.setOutputFlags
pkgs.haskellPackages.dhall.setupCompilerEnvironmentPhase
pkgs.haskellPackages.dhall.src
pkgs.haskellPackages.dhall.stdenv
pkgs.haskellPackages.dhall.system
pkgs.haskellPackages.dhall.type
pkgs.haskellPackages.dhall.userHook
pkgs.haskellPackages.dhall.version

nix-repl> pkgs.haskellPackages.dhall.meta.<TAB>
pkgs.haskellPackages.dhall.meta.description
pkgs.haskellPackages.dhall.meta.homepage
pkgs.haskellPackages.dhall.meta.license
pkgs.haskellPackages.dhall.meta.outputsToInstall
pkgs.haskellPackages.dhall.meta.platforms
pkgs.haskellPackages.dhall.meta.position

nix-repl> pkgs.haskellPackages.dhall.meta.license
{ fullName = "BSD 3-clause \"New\" or \"Revised\" License"; shortName = "bsd3"; spdxId = "BSD-3-Clause"; url = "http://spdx.org/licenses/BSD-3-Clause"; }

nix-repl> bsd3HaskellPackages = pkgs.lib.filterAttrs (_: pkg: builtins.tryEval (pkg.meta.license.spdxId or "None") == { success = true; value = "BSD-3-Clause"; }) pkgs.haskellPackages

nix-repl> pkgs.haskellPackages.<TAB>
Display all 12025 possibilities? (y or n)
pkgs.haskellPackages.3d-graphics-examples
pkgs.haskellPackages.3dmodels
pkgs.haskellPackages.4Blocks
pkgs.haskellPackages.AAI
pkgs.haskellPackages.ABList
pkgs.haskellPackages.AC-Angle
pkgs.haskellPackages.AC-Boolean
pkgs.haskellPackages.AC-BuildPlatform
pkgs.haskellPackages.AC-Colour
pkgs.haskellPackages.AC-EasyRaster-GTK
...

nix-repl> bsd3HaskellPackages.<TAB>
Display all 7200 possibilities? (y or n)
bsd3HaskellPackages.3d-graphics-examples
bsd3HaskellPackages.4Blocks
bsd3HaskellPackages.ABList
bsd3HaskellPackages.AC-Angle
bsd3HaskellPackages.AC-Boolean
bsd3HaskellPackages.AC-BuildPlatform
bsd3HaskellPackages.AC-Colour
bsd3HaskellPackages.AC-EasyRaster-GTK
bsd3HaskellPackages.AC-HalfInteger
bsd3HaskellPackages.AC-MiniTest
bsd3HaskellPackages.AC-PPM
bsd3HaskellPackages.AC-Random
...

```

# Interlude: Minimal NixOps system

In order to live demo the examples I will use NixOps

We'll start from the following blank VirtualBox system as the running example:

```nix
# Note: `nixops` does not yet have a good way to pin `nixpkgs`, which is one of
# many reasons that we don't use it
{ machine = { config, pkgs, ... }: {
    deployment = {
      targetEnv = "virtualbox";

      virtualbox = {
        headless = true;

        memorySize = 1024;

        vcpu = 2;
      };
    };
  };
}
```

```bash
$ nixops create --deployment test test.nix
$ nixops deploy --deployment test
```

# How to install executables

Set the `environment.systemPackages` NixOS option:

```nix
{ machine = { config, pkgs, ... }: {
    deployment = {
      targetEnv = "virtualbox";

      virtualbox = {
        headless = true;

        memorySize = 1024;

        vcpu = 2;
      };
    };

    environment.systemPackages = [ pkgs.tmux ];
  };
}
```

... and redeploy:

```
$ nixops deploy --deployment test
building all machine configurations...
these derivations will be built:
  /nix/store/ksbqnj74gl0szcgip41sjjw87b9sqy8l-system-path.drv
  /nix/store/jgarzkgz97akgc3in7alcjk1g4zx9adc-dbus-1.drv
  /nix/store/mz7csrwln06a75qz61kn0gl0mq469wrf-unit-dbus.service.drv
  /nix/store/b2sn7a3g12drilj6xl6lfb4mwbnyfbm5-user-units.drv
  /nix/store/g0icz19ja8gan3w2djmkfhsqbkps1177-unit-systemd-fsck-.service.drv
  /nix/store/sxhd3s8mwjr98w2yzgv8b94gspkamk3x-unit-polkit.service.drv
  /nix/store/fbgy9awp50dmpi47mhng5x9ghwsb8yi6-system-units.drv
  /nix/store/d77ynl7qrl57b41v4cz8qv3yzsp9mf0z-etc.drv
  /nix/store/a54sjdr31clryrapjq660z8w6dbwyr56-nixos-system-machine-17.09pre117493.ae5c5c06805.drv
  /nix/store/ffry8iry2kkbd2y04414q2a60n6m5zw0-nixops-machines.drv
these paths will be fetched (0.23 MiB download, 0.61 MiB unpacked):
  /nix/store/400dm9c0dkgl61h1kp6yzsazla4289g6-tmux-2.6-man
  /nix/store/rbfzppnrk9k1gmr6jxi44am8f4wc1vc2-tmux-2.6
fetching path ‘/nix/store/400dm9c0dkgl61h1kp6yzsazla4289g6-tmux-2.6-man’...
fetching path ‘/nix/store/rbfzppnrk9k1gmr6jxi44am8f4wc1vc2-tmux-2.6’...

*** Downloading ‘https://cache.nixos.org/nar/17f44bak1wgs64ffmdkg6bc1pzjqlj7s4cscwsjr1zqj7hpl7mwm.nar.xz’ (signed by ‘cache.nixos.org-1’) to ‘/nix/store/rbfzppnrk9k1gmr6jxi44am8f4wc1vc2-tmux-2.6’...

*** Downloading ‘https://cache.nixos.org/nar/0n2f37cqfaj1hinfydn5shqqikzhngncbf9c9mxkh7sdrisvg60s.nar.xz’ (signed by ‘cache.nixos.org-1’) to ‘/nix/store/400dm9c0dkgl61h1kp6yzsazla4289g6-tmux-2.6-man’...
  % Total    % Received % Xferd  Ave  % Torage Speetal    %d   Time    Time     Time  Received % Xferd  Average  Current
                 Speed   Time               Ti     Dload  mUpload   Te     Totalime  Cu   Spent rrent
    Le       ft  Speed
  0     0       0     0    Dload  Upload      Total    0     0Spent          0      0 -Left  Speed
  0     0-:--:--       0  0     0    0     0      0      0 --:--:-- --:--:-- --:100 32192  100 32192    0     0  32192      0  0:00:01 --:--:--  0:00:01 69529

100  208k  100  208k    0     0   208k      0  0:00:01  0:00:01 --:--:--  149k

waiting for locks or build slots...
copying 6 missing paths (0.64 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/ksbqnj74gl0szcgip41sjjw87b9sqy8l-system-path.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/41h3pr6qwczrmkf5par48za0sgi6kr4k-system-path’
collision between `/nix/store/pbd9sppw6v0hw4bbmpybdg3f3l9p65fk-mount.vboxsf/bin/mount.vboxsf' and `/nix/store/d3zfpazlgzaid8n3jiy76xcy33ym6p7s-VirtualBox-GuestAdditions-5.1.32-4.9.86/bin/mount.vboxsf'
created 4710 symlinks in user environment
install-info: warning: no info dir entry in `/nix/store/41h3pr6qwczrmkf5par48za0sgi6kr4k-system-path/share/info/time.info'
waiting for locks or build slots...
copying 1 missing paths (0.00 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/jgarzkgz97akgc3in7alcjk1g4zx9adc-dbus-1.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/kyayg0sx0frgpq6b7z9ghdpsp5vgabkn-dbus-1’
copying 1 missing paths (0.00 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/sxhd3s8mwjr98w2yzgv8b94gspkamk3x-unit-polkit.service.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/2dyax4cf42hxs62n4nnidd4pvr10zl4y-unit-polkit.service’
copying 1 missing paths (0.00 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/mz7csrwln06a75qz61kn0gl0mq469wrf-unit-dbus.service.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/p98k8045h4rasq5prrdfhbb18sv257zq-unit-dbus.service’
copying 1 missing paths (0.00 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/b2sn7a3g12drilj6xl6lfb4mwbnyfbm5-user-units.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/j8byx3a4vk4bjfd263caz403c67fhajq-user-units’
copying 1 missing paths (0.00 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/g0icz19ja8gan3w2djmkfhsqbkps1177-unit-systemd-fsck-.service.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/jim6fbhib75mszxgig3w0lwfwasvjvcs-unit-systemd-fsck-.service’
copying 1 missing paths (0.02 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/fbgy9awp50dmpi47mhng5x9ghwsb8yi6-system-units.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/bihql6brly8njvij36snhnkvfvfiyvqy-system-units’
copying 1 missing paths (0.01 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/d77ynl7qrl57b41v4cz8qv3yzsp9mf0z-etc.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/lzdyyd9j2clh70jrcnvn676kykkd5xcm-etc’
copying 1 missing paths (0.02 MiB) to ‘gabriel@hydra-slave03’...
building ‘/nix/store/a54sjdr31clryrapjq660z8w6dbwyr56-nixos-system-machine-17.09pre117493.ae5c5c06805.drv’ on ‘gabriel@hydra-slave03’
building path(s) ‘/nix/store/abl49b43m7qhk3a7qbh3zqh1frvqb6zx-nixos-system-machine-17.09pre117493.ae5c5c06805’
building path(s) ‘/nix/store/qf7f9ccrwwfxh11dwnx72h8caiv4wqnc-nixops-machines’
machine> copying closure...
machine> copying 11 missing paths (2.07 MiB) to ‘root@192.168.56.102’...
test> closures copied successfully
machine> updating GRUB 2 menu...
machine> activating the configuration...
machine> setting up /etc...
machine> setting up tmpfiles
machine> reloading the following units: dbus.service
machine> the following new units were started: get-vbox-nixops-client-key.service
machine> activation finished successfully
test> deployment finished successfully
```

Now verify:

```
$ nixops ssh --deployment test machine

[root@machine:~]# tmux --help
usage: tmux [-2CluvV] [-c shell-command] [-f file] [-L socket-name]
            [-S socket-path] [command [flags]]
```

# Uninstall software

Undo the option change:

```nix
{ machine = { config, pkgs, ... }: {
    deployment = {
      targetEnv = "virtualbox";

      virtualbox = {
        headless = true;

        memorySize = 1024;

        vcpu = 2;
      };
    };

    environment.systemPackages = [ ];
  };
}
```

... and redeploy:

```
$ nixops deploy --deployment test
building all machine configurations...
machine> copying closure...
test> closures copied successfully
machine> updating GRUB 2 menu...
machine> activating the configuration...
machine> setting up /etc...
machine> setting up tmpfiles
machine> reloading the following units: dbus.service
machine> the following new units were started: get-vbox-nixops-client-key.service
machine> activation finished successfully
test> deployment finished successfully

$ nixops ssh --deployment test machine
Last login: Wed Jul 11 02:40:07 2018 from 192.168.56.1

[root@machine:~]# tmux --help
tmux: command not found
```

# Garbage collection

Note that `tmux` is still there on the machine:

```
[root@machine:~]# ls -d /nix/store/*tmux*
/nix/store/400dm9c0dkgl61h1kp6yzsazla4289g6-tmux-2.6-man
/nix/store/rbfzppnrk9k1gmr6jxi44am8f4wc1vc2-tmux-2.6
```

It's just not on the `PATH` and if we run a garbage collection it will disappear

For a NixOS system you can delete everything except the latest deploy using:

```bash
$ nix-collect-garbage --delete-old
```

# Browsing NixOS options (Web)

This page is pretty good:

[https://nixos.org/nixos/options.html](https://nixos.org/nixos/options.html)

Main downsides:

* does not necessarily match our revision of `nixpkgs`
* does not include the new options that we've defined

There might be a way to generate this for our system, but we haven't checked

# Browsing NixOS options (REPL)

We'll go back to our (non-NixOps) NixOS configuration and make one change:

```nix
let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
     rev          = "74286ec9e76be7cd00c4247b9acb430c4bd9f1ce";
     sha256       = "0njb3qd2wxj7gil8y61lwh7zacmvr6zklv67w5zmvifi1fvalvdg";
     outputSha256 = "13ydgpzl5nix4gc358iy9zjd5nrrpbpwpxmfhis4aai2zmkja3ak";
  };
  
  nixos = import "${nixpkgs}/nixos" {
    system = "x86_64-linux";

    configuration = {
      boot.loader.grub.devices = [ "/dev/sda" ];

      fileSystems."/".device = "/dev/disk/by-label/root";

      services.openssh.enable = true;

      users = {
        mutableUsers = false;

        users.gabriel = {
          isNormalUser = true;

          extraGroups = [ "wheel" ];

          openssh.authorizedKeys.keys =
            [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICz3QpMYr+cDw4XcZ1di09pT7FTN/pa01RcjJLjyIg/+ example@example.com" ];
        };
      };
    };
  };

in
# nixos.system  ← Delete the `.system`
  nixos
```

Now we can browse both available options and what their final setting is:

```
nix-repl> nixos = import ./test.nix
```

Available options are located underneath the `options` attribute:

```
nix-repl> nixos.options.<TAB>
nixos.options._definedNames      nixos.options.nesting
nixos.options._module            nixos.options.networking
nixos.options.assertions         nixos.options.nix
nixos.options.boot               nixos.options.nixpkgs
nixos.options.config             nixos.options.passthru
nixos.options.containers         nixos.options.power
nixos.options.docker-containers  nixos.options.powerManagement
nixos.options.dysnomia           nixos.options.programs
nixos.options.ec2                nixos.options.security
nixos.options.environment        nixos.options.services
...

nix-repl> nixos.options.environment.<TAB>
nixos.options.environment._definedNames
nixos.options.environment.binsh
nixos.options.environment.blcr
nixos.options.environment.checkConfigurationOptions
nixos.options.environment.enableBashCompletion
nixos.options.environment.enableDebugInfo
nixos.options.environment.etc
nixos.options.environment.extraInit
...

nix-repl> nixos.options.environment.systemPackages.<TAB>
nixos.options.environment.systemPackages._type
nixos.options.environment.systemPackages.declarations
nixos.options.environment.systemPackages.default
nixos.options.environment.systemPackages.definitions
nixos.options.environment.systemPackages.description
nixos.options.environment.systemPackages.example
nixos.options.environment.systemPackages.files
nixos.options.environment.systemPackages.isDefined
nixos.options.environment.systemPackages.loc
nixos.options.environment.systemPackages.options
nixos.options.environment.systemPackages.type
nixos.options.environment.systemPackages.value

nix-repl> nixos.options.environment.systemPackages.description
"The set of packages that appear in\n/run/current-system/sw.  These packages are\nautomatically available to all users, and are\nautomatically updated every time you rebuild the system\nconfiguration.  (The latter is the main difference with\ninstalling them in the default profile,\n<filename>/nix/var/nix/profiles/default</filename>.\n"

```

The final values of options are located underneath the `config` attribute:

```
nix-repl> nixos.config.environment.systemPackages
[ «derivation /nix/store/7iw5dmjals0mq69rfmrkabm2wm5yhm83-nixos-container.drv» «derivation /nix/store/rmv4afrb7yigx38qdmkryjj8m024v5yy-mdadm-3.3.4.drv» «derivation /nix/store/ciwl5lgjir2q03509j9h4sjj52zxrbip-bind-9.11.2.drv» «derivation /nix/store/yn6ff71r2wjd91qv17mjn23c4kmhkbh8-iproute2-4.13.0.drv» «derivation /nix/store/40ifnkkkj5yk7ywxlr5cc8icy5cv4zsi-iputils-20161105.drv» «derivation /nix/store/f1z8y2xxk05zrrjc359017zq1r04492c-net-tools-1.60_p20161110235919.drv» «derivation /nix/store/042hm21kdvf0h41a6lp46zhbazaji2i4-openresolv-3.9.0.drv» «derivation /nix/store/9kks2216liypc7zlk65gxn9l9vymp36h-lvm2-2.02.173.drv» «derivation /nix/store/zxnh5g3xrfvazpp3s4d4wyrssi6wx7yd-kbd-2.0.4.drv» «derivation /nix/store/q467s759hcxyxmnps9dhs6wiaxfr9f2y-fuse-3.2.0.drv» «derivation /nix/store/pw4whx4sac4fvfhaf76al7irrnrbzg9r-fuse-2.9.7.drv» «derivation /nix/store/xgi13ddrp6xqc4wndvh1f058jzi0cw0y-e2fsprogs-1.43.4.drv» «derivation /nix/store/rz5lm213qkpdmvs6j183h4f92253d3yr-dosfstools-4.1.drv» «derivation /nix/store/gi812lf5ycslqja88b9v6m5dvpiqlkra-cpupower-4.9.76.drv» «derivation /nix/store/fz3hnsm7d7awfy7a066hmcxlyv50spja-bcache-tools-1.0.7.drv» «derivation /nix/store/d71x8f517lf7qpcbaqfbwvz40gg04bhn-systemd-234.drv» «derivation /nix/store/gklikx4nd98l68x6zbmfdj214pmq22lb-kmod-24.drv» «derivation /nix/store/p4qazbw5k22w2rm2w92z4raqar4jvs0c-grub-2.02.drv» «derivation /nix/store/30ai1f2yc931qnr2vp93l7drrg299ap8-kexec-tools-2.0.14.drv» «derivation /nix/store/ic336qyy47pva6x59igs1imingas8fyi-dbus-1.10.22.drv» «derivation /nix/store/ic336qyy47pva6x59igs1imingas8fyi-dbus-1.10.22.drv» «derivation /nix/store/5g9z4myfck9jn8d9b5id2xky9vbg2z7z-iptables-1.6.1.drv» «derivation /nix/store/lz9lmcmssrm8d65drcd3zw1r4qvz1709-dhcpcd-6.11.5.drv» «derivation /nix/store/wdpmgr7xlm66skday4xqgfhwr9sp0xy8-nixos-manual.drv» «derivation /nix/store/jwpayb7w8jnjny2jmqk21yx796vb0124-nixos-help.drv» «derivation /nix/store/sgxdqdn0nzp4ihmj5nfpbrcbmbxkc8dz-nixos-manpages.drv» «derivation /nix/store/kysh9fq98y0h1svqwgr5883xra9vhjx0-udisks-2.1.6.drv» «derivation /nix/store/0b49rnsmf48bqcf0jzrhc5j6gyf7xpdn-alsa-utils-1.1.4.drv» «derivation /nix/store/a5rkgq2mwrhfdjywq3ql6py7brc1falb-sudo-1.8.20p2.drv» «derivation /nix/store/5ipj9wayz0fip2d3kynyal37jz7vvfps-polkit-0.113.drv» «derivation /nix/store/5ipj9wayz0fip2d3kynyal37jz7vvfps-polkit-0.113.drv» «derivation /nix/store/g9sqsbbryays07x1qpxji4bsghmdjq13-linux-pam-1.2.1.drv» «derivation /nix/store/74jxzka16f10w0wzizji0m7m2sc5yxa2-bash-4.4-p12.drv» «derivation /nix/store/5s2yv0nzzm6ic9fc4cl0mngaq3411m37-man-db-2.7.5.drv» «derivation /nix/store/9myf3zf5650h7awd6bpz4m71l9l8i677-texinfo-6.3.drv» «derivation /nix/store/sj236k3jh5aiw9i72hxrf7bykya9hq24-command-not-found.drv» «derivation /nix/store/kgrjfw7bly8532rgx3razr1qfqbm81ca-nixos-build-vms.drv» «derivation /nix/store/0ks93yc54r9qlwwjp40nr8inf3fw6fa0-nixos-prepare-root.drv» «derivation /nix/store/irrjp8jxci0lbp18p4zgdyx3zxmvs7wy-nixos-install.drv» «derivation /nix/store/7r3qhyi8cz55395lgq9c9hs6wjayj6zw-nixos-rebuild.drv» «derivation /nix/store/c4rl63nr4daywjqn8v38bv82ljjh0qrn-nixos-generate-config.drv» «derivation /nix/store/nvl1k6k92amyh0xlkpkghkz7cannpgcz-nixos-option.drv» «derivation /nix/store/g704m3gfj9s617j2d1a8hrz1xx0v5vpp-nixos-version.drv» «derivation /nix/store/74jxzka16f10w0wzizji0m7m2sc5yxa2-bash-4.4-p12.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/74jxzka16f10w0wzizji0m7m2sc5yxa2-bash-4.4-p12.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/xhk9zrx64h3lxx990lvj4y9ra3za6bns-nix-1.11.16.drv» «derivation /nix/store/izaf1gc7gk5pfk6wwjvgvnixx7vf5q94-acl-2.2.52.drv» «derivation /nix/store/nzr7kvrqs8w5kz82gw82fcisdfaf8yx4-attr-2.4.47.drv» «derivation /nix/store/74jxzka16f10w0wzizji0m7m2sc5yxa2-bash-4.4-p12.drv» «derivation /nix/store/8zvdfsy0axn5b6bizdmpp2k39p1i2px9-bzip2-1.0.6.0.1.drv» «derivation /nix/store/2rnnrqg39madfz7a9ca7a5l4xg689xv0-coreutils-8.28.drv» «derivation /nix/store/dbgdbfnf6b5n89x8a2mkvxzzrp3n077y-cpio-2.12.drv» «derivation /nix/store/kac707mpf1v5cna3svb2ivjj68b2gy34-curl-7.57.0.drv» «derivation /nix/store/c46s5pwb90a3ah2w31vjhr9bc5xn2f33-diffutils-3.6.drv» «derivation /nix/store/0pkqmyzycyppr4fmgrr1sxj9x7n7rrd7-findutils-4.6.0.drv» «derivation /nix/store/gzh9ys602mmy33fwbhwf7ja0nym4a9bl-gawk-4.1.4.drv» «derivation /nix/store/8i662wa2m2v0nsrf4hiv2hwpfksizmjr-glibc-2.25-49.drv» «derivation /nix/store/jsjy62rd7vdvz0nai9y8xm3l8w1jg0p1-gnugrep-3.1.drv» «derivation /nix/store/zd43bpqmvi974m6w4xysxrsfvpvad3kb-patch-2.7.5.drv» «derivation /nix/store/laik93kc1pqxg8yrwsvdmpl45ga71wfb-gnused-4.4.drv» «derivation /nix/store/yh7p9hyix5cs1vqjzdqjhgk01wg4hf75-gnutar-1.29.drv» «derivation /nix/store/k4nnzr6vn2iccvaay9w144havzgqmi1s-gzip-1.8.drv» «derivation /nix/store/wkkgzjwhfgr3b3wz4f5qlgslqd46axbq-xz-5.2.3.drv» «derivation /nix/store/07zwr37c2binsq48qjgq5mv262xsi2wc-less-487.drv» «derivation /nix/store/yxilyspxn5syzyynygr4ps4cwp5pn1zp-libcap-2.25.drv» «derivation /nix/store/j4kzynpmwkp2yqwzpgvmqkfg9pbjngrc-nano-2.8.7.drv» «derivation /nix/store/vkmj678fdh8irhdyqijph120n5xdk0xw-ncurses-6.0-20170902.drv» «derivation /nix/store/5g9f471wkmpmrqz4x0h1xhcrd1fd1gkl-netcat-openbsd-1.130.drv» «derivation /nix/store/k2ba2hmy8qhqv93jmibckwf0gn29x95k-nix-info.drv» «derivation /nix/store/6nyxmz3ckjwqlnfgwfgizby7nbg05y5r-openssh-7.5p1.drv» «derivation /nix/store/a02pi0r4888p79xnnh70fqzc0g6gsv7g-perl-5.24.3.drv» «derivation /nix/store/yd1vadng75ilk2qi1vi5iyf26h222g84-procps-3.3.12.drv» «derivation /nix/store/dz922vl1vci42q1yasrj1dazk9gkx2dn-rsync-3.1.2.drv» «derivation /nix/store/wfiz1nq0jlm1f8p5dgalwg09n4n87nq8-strace-4.19.drv» «derivation /nix/store/aqvmn71j0273wr1ipdnq79v6n55wzskq-shadow-4.5.drv» «derivation /nix/store/jjr9s5qc40mb062dxkqz6da039a8wqa6-time-1.7.drv» «derivation /nix/store/dhcgp9z1fih7d922fkgqys0afs3jamxq-util-linux-2.31.drv» «derivation /nix/store/0g0wnrfxa28l5dj5zihl67iqci6jghz8-which-2.21.drv» «derivation /nix/store/8c8h6c7frf261hjkvf1viqayglxim5i8-glibc-locales-2.25-49.drv» «derivation /nix/store/67b8pzxrj9y9h6j24kviqs35qwgmb77h-fontconfig-2.12.1.drv» ]

nix-repl> map (pkg: pkg.name) nixos.config.environment.systemPackages
[ "nixos-container" "mdadm-3.3.4" "bind-9.11.2" "iproute2-4.13.0" "iputils-20161105" "net-tools-1.60_p20161110235919" "openresolv-3.9.0" "lvm2-2.02.173" "kbd-2.0.4" "fuse-3.2.0" "fuse-2.9.7" "e2fsprogs-1.43.4" "dosfstools-4.1" "cpupower-4.9.76" "bcache-tools-1.0.7" "systemd-234" "kmod-24" "grub-2.02" "kexec-tools-2.0.14" "dbus-1.10.22" "dbus-1.10.22" "iptables-1.6.1" "dhcpcd-6.11.5" "nixos-manual" "nixos-help" "nixos-manpages" "udisks-2.1.6" "alsa-utils-1.1.4" "sudo-1.8.20p2" "polkit-0.113" "polkit-0.113" "linux-pam-1.2.1" "bash-interactive-4.4-p12" "man-db-2.7.5" "texinfo-interactive-6.3" "command-not-found" "nixos-build-vms" "nixos-prepare-root" "nixos-install" "nixos-rebuild" "nixos-generate-config" "nixos-option" "nixos-version" "bash-interactive-4.4-p12" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "bash-interactive-4.4-p12" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "shadow-4.5" "nix-1.11.16" "acl-2.2.52" "attr-2.4.47" "bash-interactive-4.4-p12" "bzip2-1.0.6.0.1" "coreutils-8.28" "cpio-2.12" "curl-7.57.0" "diffutils-3.6" "findutils-4.6.0" "gawk-4.1.4" "glibc-2.25-49" "gnugrep-3.1" "patch-2.7.5" "gnused-4.4" "gnutar-1.29" "gzip-1.8" "xz-5.2.3" "less-487" "libcap-2.25" "nano-2.8.7" "ncurses-6.0-20170902" "netcat-openbsd-1.130" "nix-info" "openssh-7.5p1" "perl-5.24.3" "procps-3.3.12" "rsync-3.1.2" "strace-4.19" "shadow-4.5" "time-1.7" "util-linux-2.31" "which-2.21" "glibc-locales-2.25-49" "fontconfig-2.12.1" ]
```

# Installing a long-running service

The `systemd.services` option hierarchy configures `systemd` services:

[https://nixos.org/nixos/options.html#systemd.services.%3Cname%3E](https://nixos.org/nixos/options.html#systemd.services.%3Cname%3E)

```
{ machine = { config, pkgs, ... }: {
    deployment = {
      targetEnv = "virtualbox";

      virtualbox = {
        headless = true;

        memorySize = 1024;

        vcpu = 2;
      };
    };

    networking.firewall.allowedTCPPorts = [ 3000 ];

    systemd.services.static-file-server = {
      wantedBy = [ "multi-user.target" ];

      script = "${pkgs.haskellPackages.wai-app-static}/bin/warp";
    };
  };
}
```

# Output

```
$ nixops deploy --deployment test
...
$ nixops ssh --deployment test machine
[root@machine:~]# systemctl status static-file-server
● static-file-server.service
   Loaded: loaded (/nix/store/k17ic560zay3qi4can4acfwd7vak5va9-unit-static-file-
   Active: active (running) since Wed 2018-07-11 18:27:17 UTC; 18s ago
 Main PID: 2253 (static-file-ser)
    Tasks: 2 (limit: 4915)
{ machine = { config, pkgs, ... }: {
   CGroup: /system.slice/static-file-server.service
           ├─2253 /nix/store/jgw8hxx7wzkyhb2dr9hwsd9h2caaasdc-bash-4.4-p12/bin/b
           └─2259 /nix/store/j6qg7rqcrdnnn0pg0saaw8xbqkp0q4nb-wai-app-static-3.1

Jul 11 18:27:17 machine systemd[1]: Started static-file-server.service.
```

# Installing a periodic service

Just add the `startAt` option:

```
{ machine = { config, pkgs, ... }: {
    deployment = {
      targetEnv = "virtualbox";

      virtualbox = {
        headless = true;

        memorySize = 1024;

        vcpu = 2;
      };
    };

    systemd.services.periodic-hello = {
      wantedBy = [ "multi-user.target" ];

      script = "${pkgs.coreutils}/bin/echo hello";

      startAt = "minutely";
    };
  };
}
```

# Output

```
$ nixops deploy --deployment test
...
$ nixops ssh --deployment test machine
[root@machine:~]# journalctl -f -u periodic-hello
...
Jul 11 18:32:09 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:32:10 machine periodic-hello-start[2940]: hello
Jul 11 18:33:09 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:33:09 machine periodic-hello-start[2987]: hello
Jul 11 18:34:38 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:34:38 machine periodic-hello-start[2992]: hello
Jul 11 18:35:17 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:35:17 machine periodic-hello-start[2997]: hello

```

# Uninstalling a service

Set `enable = false` to keep the code but not install the service definition:

```nix
{ machine = { config, pkgs, ... }: {
    deployment = {
      targetEnv = "virtualbox";

      virtualbox = {
        headless = true;

        memorySize = 1024;

        vcpu = 2;
      };
    };

    systemd.services.periodic-hello = {
      enable = false;

      wantedBy = [ "multi-user.target" ];

      script = "${pkgs.coreutils}/bin/echo hello";

      startAt = "minutely";
    };
  };
}
```

# Output

```
$ nixops deploy --deployment test
...
$ nixops ssh --deployment test machine
[root@machine:~]# systemctl status periodic-hello
● periodic-hello.service
   Loaded: masked (/dev/null; bad)
   Active: inactive (dead)

Jul 11 18:32:09 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:32:10 machine periodic-hello-start[2940]: hello
Jul 11 18:33:09 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:33:09 machine periodic-hello-start[2987]: hello
Jul 11 18:34:38 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:34:38 machine periodic-hello-start[2992]: hello
Jul 11 18:35:17 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:35:17 machine periodic-hello-start[2997]: hello
Jul 11 18:36:23 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:36:23 machine periodic-hello-start[3011]: hello
```

# Disabling the service

You can deploy the service definition but not automatically run the service

Just delete `wantedBy` and the service will be installed but not run by default

```
```

# Output

```
$ nixops deploy --deployment test
...
$ nixops ssh --deployment test machine
[root@machine:~]# systemctl status periodic-hello
● periodic-hello.service
   Loaded: loaded (/nix/store/sk7hyx3073p235h7hwdfifxh7xndivhs-unit-periodic-hel
   Active: inactive (dead)

Jul 11 18:32:09 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:32:10 machine periodic-hello-start[2940]: hello
Jul 11 18:33:09 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:33:09 machine periodic-hello-start[2987]: hello
Jul 11 18:34:38 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:34:38 machine periodic-hello-start[2992]: hello
Jul 11 18:35:17 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:35:17 machine periodic-hello-start[2997]: hello
Jul 11 18:36:23 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:36:23 machine periodic-hello-start[3011]: hello

[root@machine:~]# systemctl start periodic-hello

[root@machine:~]# systemctl status periodic-hello
● periodic-hello.service
   Loaded: loaded (/nix/store/sk7hyx3073p235h7hwdfifxh7xndivhs-unit-periodic-hel
   Active: inactive (dead) since Wed 2018-07-11 18:41:01 UTC; 1s ago
  Process: 3586 ExecStart=/nix/store/y6cgr05gx996ndjcj3y122mjfg8y9mzn-unit-scrip
 Main PID: 3586 (code=exited, status=0/SUCCESS)

Jul 11 18:41:01 machine systemd[1]: Started periodic-hello.service.
Jul 11 18:41:01 machine periodic-hello-start[3586]: hello
```

# Containers

NixOS provides options for using `systemd-nspawn` containers

Yes, `systemd` natively supports containers 🤯

See the `containers` option hierarchy:

[https://nixos.org/nixos/options.html#containers](https://nixos.org/nixos/options.html#containers)

In particular, the key option is `containers.<name>.config`

That lets you convert an arbitrary NixOS configuration to a container

Also supports features like volume mounts, capabilities, port forwarding, etc.

# Run our services inside of `systemd-nspawn` containers

```nix
{ machine = { config, pkgs, ... }: {
    deployment = {
      targetEnv = "virtualbox";

      virtualbox = {
        headless = true;

        memorySize = 1024;

        vcpu = 2;
      };
    };

    containers = {
      foo = {
        autoStart = true;

        config = {
          systemd.services.periodic-hello = {
            wantedBy = [ "multi-user.target" ];

            script = "${pkgs.coreutils}/bin/echo hello";

            startAt = "minutely";
          };
        };
      };

      bar = {
        autoStart = true;

        config = {
          networking.firewall.allowedTCPPorts = [ 3000 ];

          systemd.services.static-file-server = {
            wantedBy = [ "multi-user.target" ];

            script = "${pkgs.haskellPackages.wai-app-static}/bin/warp";
          };
        };

        forwardPorts = [
          { containerPort = 3000; hostPort = 80; protocol = "tcp"; }
        ];
      };
    };
  };
}
```


# Output

```
[root@machine:~]# systemctl status container@foo
● container@foo.service - Container 'foo'
   Loaded: loaded (/nix/store/32ln4z46wnv94ycgbmm2fqigzca9ilk2-unit-container-fo
   Active: active (running) since Wed 2018-07-11 18:51:38 UTC; 23s ago
  Process: 4805 ExecStartPost=/nix/store/s19bimchhkhm3p2sdxw0cf1aq5larhv7-unit-s
  Process: 4231 ExecStartPre=/nix/store/6q2z4wjvqkd7jskh9hdnkp6a2hpid8ax-unit-sc
 Main PID: 4248 (systemd-nspawn)
   Status: "Container running: Startup finished in 2.858s."
    Tasks: 15 (limit: 4915)
   CGroup: /system.slice/system-container.slice/container@foo.service
           ├─4248 /nix/store/3b1rsrlxxddlzlqg0hgsb2941k1hsb7f-systemd-234/bin/sy
           ├─init.scope
           │ └─4274 systemd
           └─system.slice
             ├─console-getty.service
             │ └─4804 agetty --login-program /nix/store/ai0cqsx1j1mrd5wp1qx2hr27
             ├─dbus.service
             │ └─4802 /nix/store/yzjgy81f766ff6m2kfmh3f1q9lahrm8l-dbus-1.10.22/b
             ├─nscd.service
             │ └─4783 nscd
             ├─systemd-journald.service
             │ └─4737 /nix/store/3b1rsrlxxddlzlqg0hgsb2941k1hsb7f-systemd-234/li
             └─systemd-logind.service
               └─4801 /nix/store/3b1rsrlxxddlzlqg0hgsb2941k1hsb7f-systemd-234/li

Jul 11 18:51:38 machine container foo[4248]: [  OK  ] Started Permit User Sessio
Jul 11 18:51:38 machine container foo[4248]: [  OK  ] Started Console Getty.
Jul 11 18:51:38 machine container foo[4248]: [  OK  ] Reached target Login Promp
Jul 11 18:51:38 machine container foo[4248]: [  OK  ] Started Login Service.
Jul 11 18:51:38 machine container foo[4248]: [  OK  ] Reached target Multi-User
Jul 11 18:51:38 machine systemd[1]: Started Container 'foo'.
Jul 11 18:51:39 machine container foo[4248]: [2B blob data]
Jul 11 18:51:39 machine container foo[4248]: [1B blob data]
Jul 11 18:51:39 machine container foo[4248]: <<< Welcome to NixOS 17.09pre117493
Jul 11 18:51:39 machine container foo[4248]: [1B blob data]

[root@machine:~]# nixos-container root-login foo

[root@foo:~]# systemctl status periodic-hello
● periodic-hello.service
   Loaded: loaded (/nix/store/sk7hyx3073p235h7hwdfifxh7xndivhs-unit-periodic-hel
   Active: inactive (dead) since Wed 2018-07-11 18:52:28 UTC; 10s ago
  Process: 253 ExecStart=/nix/store/y6cgr05gx996ndjcj3y122mjfg8y9mzn-unit-script
 Main PID: 253 (code=exited, status=0/SUCCESS)

Jul 11 18:52:28 foo systemd[1]: Started periodic-hello.service.
Jul 11 18:52:28 foo periodic-hello-start[253]: hello

[root@machine:~]# nixos-container root-login bar

[root@foo:~]# exit
logout

[root@bar:~]# systemctl status static-file-server
● static-file-server.service
   Loaded: loaded (/nix/store/k17ic560zay3qi4can4acfwd7vak5va9-unit-static-file-
   Active: active (running) since Wed 2018-07-11 18:51:36 UTC; 2min 43s ago
 Main PID: 229 (static-file-ser)
   CGroup: /system.slice/system-container.slice/container@bar.service/system.sli
           ├─229 /nix/store/jgw8hxx7wzkyhb2dr9hwsd9h2caaasdc-bash-4.4-p12/bin/ba
           └─232 /nix/store/j6qg7rqcrdnnn0pg0saaw8xbqkp0q4nb-wai-app-static-3.1.

Jul 11 18:51:36 bar systemd[1]: Started static-file-server.service.
```

# Configuring environment variables

* Globally: `environment.variables`
* Per-service: `systemd.services.<name>.environment`
* Per-package: Wrap the executable using `makeWrapper`

# Seeing what changed

You can use `nix-diff` to compare two system derivations to see what changed

```
$ nix-instantiate --attr prod release.nix
/nix/store/d7pi1ghk78y3s3h69kq1i81y0dp6c3hi-nixos-system-nixos-17.09-awake.drv

$ nix-instantiate --attr prod-dogfood release.nix
/nix/store/v75hkkx4zfdq689bls4jznj1ar46dmwa-nixos-system-nixos-17.09-awake.drv

$ nix-diff /nix/store/d7pi1ghk78y3s3h69kq1i81y0dp6c3hi-nixos-system-nixos-17.09-awake.drv /nix/store/v75hkkx4zfdq689bls4jznj1ar46dmwa-nixos-system-nixos-17.09-awake.drv
- /nix/store/d7pi1ghk78y3s3h69kq1i81y0dp6c3hi-nixos-system-nixos-17.09-awake.drv:{out}
+ /nix/store/v75hkkx4zfdq689bls4jznj1ar46dmwa-nixos-system-nixos-17.09-awake.drv:{out}
• The input named `etc` differs
  - /nix/store/z0bbrhhx7lyqdhd89azfl14rjmbqws5p-etc.drv:{out}
  + /nix/store/pkqsxlrq1nriz2jh4prpnyghpx9q952c-etc.drv:{out}
  • The set of input names do not match:
      + akon-authorized_keys
      + alex-authorized_keys
      + andrew-authorized_keys
      + bruce-authorized_keys
      + daniel-authorized_keys
      + dash-authorized_keys
      + david-authorized_keys
      + gabriel-authorized_keys
      + gershom-authorized_keys
      + ivan-authorized_keys
      + james-authorized_keys
      + jcarey-authorized_keys
      + jeff-authorized_keys
      + jenkins-authorized_keys
      + joel-authorized_keys
      + keith-authorized_keys
      + matt-authorized_keys
      + nathan-authorized_keys
      + nyk-authorized_keys
      + parnell-authorized_keys
      + py-authorized_keys
      + raju-authorized_keys
      + rajug-authorized_keys
      + ram-authorized_keys
      + raman-authorized_keys
      + reid-authorized_keys
      + ry-authorized_keys
      + sk-authorized_keys
      + vincent-authorized_keys
  • The input named `dbus-1` differs
    - /nix/store/gxink1rdb0w59v30dc1ldvqzadmsz5zb-dbus-1.drv:{out}
    + /nix/store/rqgfzlh1w70qhhr0zxx71dryv5amccm4-dbus-1.drv:{out}
    • The input named `system-path` differs
      - /nix/store/mraqylhrxrfy8f9fr0vb3rc0mbz5pwfz-system-path.drv:{out}
      + /nix/store/nw2smy1vsxrrnrw6rzw7iarlip4nfas6-system-path.drv:{out}
      • The environments do not match:
          pkgs=''
            ...
```

# Applications of `nix-diff`

You can diff `prod` before and after your change to see what affect it has

You can verify that a refactor is behavior-preserving (same has)

# Filesystem hierarchy

Nix has a much more restricted filesystem hierarchy

* `/run/booted-system` - Symlink to booted system
* `/run/current-system` - Symlink to currently running system
* `/nix/store` - All binaries and text files the system depends on live here
    * `/nix/store/*.drv` - Derivations
    * `/nix/store/*` - Outputs (not ending in `.drv`)
* `/nix/var` - Ephemeral data for Nix operations
    * `/nix/var/nix/profiles/system*` - Symlinks to system generations
    * `/nix/var/nix/gcroots` - Garbage collection roots
* `/etc/` - Still system configuration directory
    * `/etc/static` immutable directory containing system configuration files
        * Many things in `/etc/` are a symlink into `/etc/static`
    * Still mutable for anything that's not a symlink to an immutable file
* `/bin` - Only has `/bin/sh` (Superseded by `/nix/store`)
* `/usr` - Only has `/usr/bin/env` (Superseded by `/nix/store`)
* `/lib` - Gone (Superseded by `/nix/store`)
* `/home` - Mutable, do whatever you want within your respective directory
* Everything else is basically the same

```
$ ls -l /etc/systemd
total 0
lrwxrwxrwx 1 root root 33 Jul 11 18:09 journald.conf -> /etc/static/systemd/journald.conf
lrwxrwxrwx 1 root root 31 Jul 11 18:09 logind.conf -> /etc/static/systemd/logind.conf
lrwxrwxrwx 1 root root 30 Jul 11 18:09 sleep.conf -> /etc/static/systemd/sleep.conf
lrwxrwxrwx 1 root root 26 Jul 11 18:09 system -> /etc/static/systemd/system
lrwxrwxrwx 1 root root 31 Jul 11 18:09 system.conf -> /etc/static/systemd/system.conf
lrwxrwxrwx 1 root root 37 Jul 11 18:09 system-generators -> /etc/static/systemd/system-generators
lrwxrwxrwx 1 root root 34 Jul 11 18:09 timesyncd.conf -> /etc/static/systemd/timesyncd.conf
lrwxrwxrwx 1 root root 24 Jul 11 18:09 user -> /etc/static/systemd/user
lrwxrwxrwx 1 root root 29 Jul 11 18:09 user.conf -> /etc/static/systemd/user.conf

$ tree /usr
/usr
└── bin
    └── env -> /nix/store/r90xqqmd36fv3s53bf92s3vxhdnbwfn7-coreutils-8.28/bin/env

1 directory, 1 file
```

# `systemd`

NixOS leans very heavily on `systemd`:

* Services are defined using `systemd`
* Containers are defined using `systemd`

Learning `systemd` is a must for effective NixOS administration

Key commands are:

* `systemctl`
* `journalctl`

# What is wrong with how we deploy production appliances?

Short answer: not documented so we had no idea until too late

Longer answer: Some of our NixOS options store secrets in plaintext

Instead we should not use Nix/NixOS or the `/nix/store` to manage secrets

Example: `/root/.aws/credentials` instead of `services.aws.*` options

Once fixed, that unlocks:

* 100% binary deploys
    * No Nix code on appliances
    * No cache misses
    * Reliable offline deploys
* Pushbutton deploys from developer laptops
    * No more channels
    * No more `/etc/nixos/configuration.{json,nix}`
* Nix generating Nix code
    * No need to manually run `update-config` to test changes
    * No more Jenkins jobs in CI
    * No need to check generate Nix code into version control

# Conclusion

NixOS in production is designed to be declarative and immutable

NixOS in production designed around doing all modifications via Nix source code

You can edit system via command line, but not recommended for production

We should be using NixOS containers instead of Docker containers

We should be locally building binary deploys of customer systems
