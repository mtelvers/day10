A project's pin-depends are resolved by day10: each pinned source is fetched,
its opam file read, and the package put in the repository the solver and the
build use, pinned to the commit the pin-depends named.

  $ export HOME=$PWD
  $ export GIT_AUTHOR_NAME=day10 GIT_AUTHOR_EMAIL=day10@invalid GIT_COMMITTER_NAME=day10 GIT_COMMITTER_EMAIL=day10@invalid
  $ export GIT_AUTHOR_DATE='2026-01-01T00:00:00Z' GIT_COMMITTER_DATE='2026-01-01T00:00:00Z'

  $ mkdir -p repo/packages project cache/debian-13-x86_64/base
  $ printf 'opam-version: "2.0"\n' > repo/repo
  $ pkg() {
  >   mkdir -p repo/packages/$1/$1.$2
  >   name=$1; version=$2; shift 2
  >   { echo 'opam-version: "2.0"'; for field in "$@"; do echo "$field"; done; } > repo/packages/$name/$name.$version/opam
  > }
  $ pkg ocaml 5.4.1
  $ pkg b 1.0
  $ pkg c 1.0

A source to pin, which is in no repository and depends on one that is.  Its
opam file is k.opam at the top, as dune writes it.

  $ mkgit() {
  >   mkdir -p $1 && git -C $1 init --quiet --initial-branch=main && cp $2 $1/$3 && git -C $1 add . && git -C $1 commit --quiet -m init && git -C $1 rev-parse HEAD
  > }
  $ printf 'opam-version: "2.0"\ndepends: [ "b" ]\n' > k.opam
  $ K=$(mkgit k k.opam k.opam)

  $ ci() {
  >   day10 build --dry-run --log --cache-dir cache --opam-repository repo \
  >     --os linux --os-distribution debian --os-version 13 --arch x86_64 --ocaml-version ocaml.5.4.1 project 2>&1 \
  >     | sed -e "s|$PWD|\$PWD|g" -e "s|$K|\$K|g" -e "s|${M:-none}|\$M|g" -e "s|${M2:-none}|\$M2|g"
  > }

Pinned to a commit.

  $ printf 'opam-version: "2.0"\ndepends: [ "k" ]\npin-depends: [ [ "k.dev" "git+file://%s/k#%s" ] ]\n' "$PWD" "$K" > project/real.opam
  $ ci
  [NOTE] pin-depends: k.dev -> git+file://$PWD/k#$K
  digraph opam {
    "k.dev" -> "b.1.0";
    "real.dev" -> "k.dev";
  }
  
  [NOTE] solution


Pinned to a branch, which is resolved to the commit it points at.

  $ printf 'opam-version: "2.0"\ndepends: [ "k" ]\npin-depends: [ [ "k.dev" "git+file://%s/k#main" ] ]\n' "$PWD" > project/real.opam
  $ ci
  [NOTE] pin-depends: k.dev -> git+file://$PWD/k#$K
  digraph opam {
    "k.dev" -> "b.1.0";
    "real.dev" -> "k.dev";
  }
  
  [NOTE] solution


A pinned package's own pin-depends are followed.  m keeps its opam file as a
plain opam at the top, and k now depends on it.

  $ printf 'opam-version: "2.0"\ndepends: [ "c" ]\n' > m.opam
  $ M=$(mkgit m m.opam opam)
  $ printf 'opam-version: "2.0"\ndepends: [ "b" "m" ]\npin-depends: [ [ "m.1.0" "git+file://%s/m#%s" ] ]\n' "$PWD" "$M" > k/k.opam
  $ git -C k commit --quiet -am "pin m"
  $ K=$(git -C k rev-parse HEAD)
  $ ci
  [NOTE] pin-depends: k.dev -> git+file://$PWD/k#$K
  [NOTE] pin-depends: m.1.0 -> git+file://$PWD/m#$M
  digraph opam {
    "k.dev" -> {"b.1.0" "m.1.0"}
    "m.1.0" -> "c.1.0";
    "real.dev" -> "k.dev";
  }
  
  [NOTE] solution


Two pins of one package agree when they resolve to the same commit: m's main
is $M, which is what k pins.

  $ printf 'opam-version: "2.0"\npin-depends: [ [ "m.1.0" "git+file://%s/m#main" ] ]\n' "$PWD" > project/other.opam
  $ ci
  [NOTE] pin-depends: m.1.0 -> git+file://$PWD/m#$M
  [NOTE] pin-depends: k.dev -> git+file://$PWD/k#$K
  digraph opam {
    "k.dev" -> {"b.1.0" "m.1.0"}
    "m.1.0" -> "c.1.0";
    "other.dev";
    "real.dev" -> "k.dev";
  }
  
  [NOTE] solution

Once main has moved on they disagree, and are refused rather than one picked.

  $ echo '# moved on' >> m/opam
  $ git -C m commit --quiet -am "move main"
  $ M2=$(git -C m rev-parse HEAD)
  $ ci
  [NOTE] pin-depends: m.1.0 -> git+file://$PWD/m#$M2
  [NOTE] pin-depends: k.dev -> git+file://$PWD/k#$K
  [ERROR] pin-depends disagree: other.opam pins m.1.0 to git+file://$PWD/m#$M2, and k.dev pins m.1.0 to git+file://$PWD/m#$M
  $ rm project/other.opam

A pin of one of the project's own packages leaves the project's own in place,
and its source is not fetched: k's has no z.opam, so fetching it would fail.

  $ printf 'opam-version: "2.0"\ndepends: [ "b" ]\npin-depends: [ [ "z.dev" "git+file://%s/k#%s" ] ]\n' "$PWD" "$K" > project/real.opam
  $ printf 'opam-version: "2.0"\n' > project/z.opam
  $ ci
  [WARNING] pin-depends: z.dev in real.opam is a package of this project, which is used instead
  digraph opam {
    "real.dev" -> "b.1.0";
    "z.dev";
  }
  
  [NOTE] solution

  $ rm project/z.opam

A source with no opam file where opam looks for one: k's only opam file is
k.opam, which is not n's.

  $ printf 'opam-version: "2.0"\ndepends: [ "n" ]\npin-depends: [ [ "n.dev" "git+file://%s/k#%s" ] ]\n' "$PWD" "$K" > project/real.opam
  $ ci
  [ERROR] pin-depends: cannot resolve n.dev from git+file://$PWD/k#$K, asked for by real.opam: no opam file at $K (looked for n.opam, opam, opam/n.opam, opam/opam)

Only git is resolved.

  $ printf 'opam-version: "2.0"\npin-depends: [ [ "n.dev" "https://example.invalid/n.tar.gz" ] ]\n' > project/real.opam
  $ ci
  [ERROR] pin-depends: cannot resolve n.dev from https://example.invalid/n.tar.gz, asked for by real.opam: day10 resolves only git pin-depends
