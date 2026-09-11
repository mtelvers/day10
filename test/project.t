A project's own packages are found by reading its .opam files.  Two kinds of
file look like one and are not, and both have been taken for one before.

  $ export HOME=$PWD

  $ mkdir -p repo/packages project/sub.t cache/debian-13-x86_64/base
  $ printf 'opam-version: "2.0"\n' > repo/repo
  $ pkg() {
  >   mkdir -p repo/packages/$1/$1.$2
  >   name=$1; version=$2; shift 2
  >   { echo 'opam-version: "2.0"'; for field in "$@"; do echo "$field"; done; } > repo/packages/$name/$name.$version/opam
  > }
  $ pkg ocaml 5.4.1
  $ pkg b 1.0

The project's one real package.

  $ printf 'opam-version: "2.0"\ndepends: [ "b" ]\n' > project/real.opam

macOS leaves an AppleDouble copy beside everything it puts in a tarball, so
._real.opam turns up next to real.opam.  It is not an opam file at all: it
starts with a binary header, and parsing it fails.

  $ printf 'binary-junk-not-an-opam-file' > project/._real.opam

A directory ending in .t is a cram test by dune's convention, so what is inside
it is fixture rather than project -- some projects keep empty .opam files there
for their tests to generate over.

  $ printf 'opam-version: "2.0"\n' > project/sub.t/fixture.opam

Only the real one is pinned into the solution, and neither of the others is
reported as a package that would not parse.

  $ day10 ci --dry-run --log --cache-dir cache --opam-repository repo \
  >   --os-distribution debian --os-version 13 --arch x86_64 --ocaml-version ocaml.5.4.1 project
  digraph opam {
    "real.dev" -> "b.1.0";
  }
  
  [NOTE] solution
