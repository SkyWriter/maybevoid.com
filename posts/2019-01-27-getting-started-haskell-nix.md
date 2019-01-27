---
title: Getting Started Haskell Project with Nix
---

One of the most confusing thing when starting a Haskell project
is on choosing the right toolchain. The Haskell ecosystem offers
a number of package management solutions, which may be difficult
for newcomers to choose. While most debates are about choosing
between [Stack](https://www.haskellstack.org/) and
[cabal-install](https://www.haskell.org/cabal/), here I am going
to introduce starting a Haskell project with
[Nix](https://nixos.org/nix/).

## Why Nix

Strictly speaking, Nix is a general package manager that is not just
for Haskell projects. You can use Nix to build projects written in
any language, and that includes C/C++, Haskell, and even dynamic typed
languages such as Python and Node.js. The biggest advantage of using
Nix for projects is that it is language agnostic and follows
functional programming principles such as purity.

When introducing new languages to beginners, one of the most painful
experience is to install a language toolchain on a person's computer.
Haskell is no exception, and telling people for example to uninstall
Haskell Platform and install Stack would only add more confusion.
Ideally, I'd like to _not_ care about which version of GHC is
installed on someone's computer, and whether their `cabal` binary is
linked to the same toolchain we want them to use.

Nix solves this problem by providing a universal package management
for everyone. Furthermore the purity property of nix means that
anyone can build my Haskell project on their machine regardless
of which version of GHC or cabal is installed.

Comparing to Stack, Nix also have the advantage that we can host
private Haskell packages, or even private distributions with Nix.
The expressiveness of Nix makes it possible to add or remove
packages, apply private patches to packages, use upstream or
unpublished packages, and pin some packages to specific versions.
When working on multi-language projects, Nix also makes it easy to
integrate Haskell projects to a larger system and manage them as a whole.

Having said that, setting up a project with Nix currently takes more
effort than setting up a project with Haskell-specific tools such
as Stack. This is mostly a documentation issue as Nix is not very
well documented and may be confusing in some ways. I still believe
learning to use Nix with Haskell is worth the effort, as Nix
benefits the users of a project more than the maintainers. The
knowledge for Nix is also transferrable, allowing you to build
other non-Haskell projects with Nix as well after mastering it.

## Setup

This tutorial is mainly based on Gabriel Gonzalez's guide to using [Haskell with Nix](https://github.com/Gabriel439/haskell-nix). To keep things simple
I will skip the more advanced topics and focus on starting a bare
minimal Haskell project with Nix.

The first step is to install Nix on your machine following the
instructions at [https://nixos.org/nix/](https://nixos.org/nix/).
A quick reference of the installation command is as follow:

```bash
curl https://nixos.org/nix/install | sh
```

After installing Nix, it is recommended that you install Cabal
on your system using Nix, so that you can use the `cabal` command
inside `nix-shell`.

```bash
nix-env -i cabal-install
```

Note that this may introduce some impurity to your project build step,
as you now have to instruct your users to not only install Nix, but
also Cabal globally. Later on I will introduce some ways to include
cabal directly in a pure Nix environment. However as most of
the Haskell Nix tutorials out there assume that you have Cabal
installed globally, it is a good idea to have it so that you
can follow other tutorials later on.

Also note that there is no need to install GHC on your system.
The Haskell Nix toolchain exposes GHC in both `nix-build` and
`nix-shell`.

## Quick Hacks

Often times we just want to have some quick hack with a few
package dependencies. In such cases setting up a full project
structure with Cabal may be a bit overkill. Fortunately Nix
can help us setup our desired Haskell environment quickly
with `nix-shell`.

Let's say if we want to quickly try out the `lens` library
and learn about lenses through GHCi, we can first enter
`nix-shell` the following way:

```bash
nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.lens])"
```

Nix will download the dependencies of `lens` and add the package
to GHC's include path. So if we open `ghci` inside of `nix-shell`
we can use modules from `lens` directly.

```bash
[nix-shell:~]$ ghci
Prelude> import Control.Lens
Prelude Control.Lens> ("hello","world")^._2
"world"
```

We can include multiple Haskell packages into our Nix shell,
in addition to executables from other Nix packages as well. For
example the following command would setup a shell with
both the `lens` and `mtl` Haskell packages installed,
as well as including `cabal` and `gdb` inside a pure Nix shell.

```bash
nix-shell --pure -p \
  "haskellPackages.ghcWithPackages (pkgs: with pkgs; [lens mtl])" \
  "haskellPackages.cabal-install" \
  gdb
```

If your Nix shell setup is a bit more complicated, you can create
a `shell.nix` file with something like the following:

```nix
{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    lens
    mtl
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    pkgs.gdb
    haskellPackages.cabal-install
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
```

I will go into details on Nix in some future posts, for now the
essence is that we are creating a "custom" ghc package in
Nix bundled with the packages we desired, using
`haskellPackages.ghcWithPackages`. We then use `stdenv.mkDerivation`
as the standard way of building a Nix derivation. The `buildInputs`
field contains which Nix packages we depend on, and we require
the custom GHC we have, `gdb` and `cabal-install`.

The main things for you to change is the array content of `haskellDeps`
and `nixPackages`. After that you can use `nix-shell` to enter the
custom shell you have defined:

```bash
nix-shell --pure shell.nix
```

The `--pure` flag makes sure that you can only access the packages
you have defined, so that you do not accidentally use global packages
that you have installed on your system. In normal development it is
common to not include the `--pure` flag during rapid prototyping.

When you include Haskell packages with executable in `nix-shell`,
those executables will automatically become available in your $PATH.
If you just want to gain access to the executable of a particular
Haskell package, you can also use `nix-build` to "build" the
package in the current directory:

```bash
nix-build '<nixpkgs>' --attr haskellPackages.hakyll
```

From the above example, you will get `result/bin/hakyll-init`.

## Building Projects With Nix

When building real world Haskell projects, we want to create `.nix`
files in our project directory to manage the build for us. Under the
hood, Nix would still use `cabal-install` to build the project for us.
But just as with the quick hacks, we use Nix to manage the Haskell
dependencies for us, instead of using Stack or Cabal.

For a fresh project, we first start our Haskell project using `cabal init`.
But instead of calling cabal directly, we can run it inside a Nix sandbox:

```bash
nix-shell --pure -p ghc cabal-install --run "cabal init"
```

After creating our `.cabal` file, we use the `cabal2nix` command
to generate a `default.nix` file from it.

```bash
nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix
```

The `default.nix` file would contain the dependencies and build
information extracted from the `.cabal` file. Note that you have
to regenerate `default.nix` every time you update the `.cabal`
file.

If your project contains executable that you want to distribute,
you can use `nix-build` to build the project in a pure way.
However the `default.nix` generated from `cabal2nix` expects
some arguments, and cannot be called directly from `nix-build`.
In programming sense, `default.nix` contains a function that
generates the Nix package given the expected arguments.

Fortunately the arguments are rather standard in Nix. The
reason `default.nix` is parameterized is so that it can
be used for different Nix channels or snapshots. To build
it against the default Nix stable channel, we create a
`release.nix` file that calls it with the standard arguments:

```nix
let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./default.nix { }
```

You would often see `release.nix` copied as-is to projects
and tutorials, sometimes in different names, as it is simply
a boilerplate file used together with `cabal2nix`.

Now we can build our Haskell executable with the following:

```bash
nix-build release.nix
```

The project would be compiled in a pure Nix environment, and
you should find the result executable in the `result/bin/`
directory relative to where `nix-build` is run.

The package, or derivation, from `release.nix` can then be
imported by other Nix files to build new derivations. If you
have a larger project in Nix, you can import `release.nix`
to use the generated binary to build other projects.

You can also use `nix-shell` to enter a shell with the
Haskell dependencies specified in `default.nix` installed:

```bash
nix-shell --attr env release.nix
```

The `--attr env` flag is a Haskell-specific way of getting
the shell environment from Haskell Nix packages.
You will find `ghc` and `ghci` available with the dependencies
available. Unfortunately the shell does not include `cabal` by
default. That is why we have to globally install `cabal-install`
with `nix-env -i cabal-install` earlier. Inside the Nix shell,
we can run commands such as `cabal repl` and `cabal build`,
and Cabal would load the dependencies from Nix.

If we want to enter a pure Nix shell with `cabal-install`,
we have to go through another hack and create a `shell.nix`
that derives a shell out of our `release.nix`:

```nix
{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
  ];
}
```

The `env` we get from `nix-shell --attr env release.nix` earlier
is a standard derivation with the customized GHC included in
`env.nativeBuildInputs`. When entering Nix shell, Nix would
add the executables exposed by the derivations in `buildInputs`
and `nativeBuildInputs` to our shell $PATH. So we simply make
a new derivation that extends the build inputs from `release.nix`
to add the executables we need, such as cabal install.

With the new `shell.nix`, we can now for example run `cabal repl`
inside a pure nix shell:

```bash
nix-shell --pure shell.nix --run "cabal repl"
```

### Private Packages

If you have multiple private Haskell projects, you can have them
depend on one another by importing `default.nix` and add them
to the `haskellPackages` list. This can be done with a bit of
boilerplate using `haskellPackages.override`:

```nix
haskellPackages = pkgs.haskellPackages.override {
  overrides = self: super: {
    my-haskell-project = self.callPackage path/to/project/default.nix {};
  };
};
```

The above snippet would define a new `haskellPackages` with an
additional package named `my-haskell-project`.

## Demo Project

We end this post with a demo project made available [on GitHub](https://github.com/maybevoid/maybevoid.com/tree/master/projects/2019-01-27-getting-started-haskell-nix) to demonstrate how a Haskell project can be built purely with Nix.
The project have a single dependency on the `lens` library, and exposes a module
`Demo` and an executable `haskell-nix-demo`.

The `Demo` module simply exports two strings `hello` and `world`,
extracted from a tuple using `Control.Lens`. The `haskell-nix-demo`
simply imports the strings and print out "hello world".

The key is the Nix files that define how the project is built:

  - `default.nix` is generated from `haskell-nix-demo.cabal` using `cabal2nix`.
  - `release.nix` is used to build the `haskell-nix-demo` executable.
  - `shell.nix` is used to enter a Nix shell with both Haskell dependencies and
    `cabal-install`.
  - `external.nix` is how an external project can import the library as a
    private module.

The `Makefile` contains build recipes for building the Nix files:

  - `make build` builds the project using `nix-build`.
  - `make repl` enters `cabal repl` from a pure Nix shell.
  - `make shell` enters a Nix shell.
  - `make shell-pure` enters a pure Nix shell.
  - `make external-shell` enters a pure Nix shell with the project built and installed.

The main difference between `make shell` and `make external-shell` is that
`make shell` provides a shell environment _before_ the project is built,
while `make external-shell` shows how the project can be consumed as a
Nix package. Inside `make external-shell`, both the `Demo` module and
the `haskell-nix-demo` executable are ready for use:

```bash
$ make external-shell
[nix-shell]$ haskell-nix-demo
hello world

[nix-shell]$ ghci
Prelude> import Demo
Prelude Demo> hello
"hello"
Prelude Demo> world
"world"
```

## References

  - [Gabriel Gonzalez's guide to using Haskell with Nix](https://github.com/Gabriel439/haskell-nix)
  - [Nixpkgs guide to the Haskell infrastructure](https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure)
  - [Nix integration with Cabal](https://cabal.readthedocs.io/en/latest/nix-integration.html)
  - [Nix integration with Stack](https://docs.haskellstack.org/en/stable/nix_integration/)