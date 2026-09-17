Solving needs no container, so the choices day10 makes about versions can be
driven here directly.  Each case below is one that has gone wrong before.

  $ export HOME=$PWD

A repository of its own, so the answers do not move when opam-repository does.

  $ mkdir -p repo/packages
  $ printf 'opam-version: "2.0"\n' > repo/repo
  $ pkg() {
  >   mkdir -p repo/packages/$1/$1.$2
  >   name=$1; version=$2; shift 2
  >   { echo 'opam-version: "2.0"'; for field in "$@"; do echo "$field"; done; } > repo/packages/$name/$name.$version/opam
  > }

The compiler, as opam-repository arranges it: ocaml is a version number that
depends on something providing it, rather than the other way about.

  $ pkg ocaml 5.4.1 'depends: [ "ocaml-base-compiler" {>= "5.4.1~" & < "5.4.2~"} ]'
  $ pkg ocaml 5.5.0 'depends: [ "ocaml-base-compiler" {>= "5.5.0~" & < "5.5.1~"} ]'
  $ pkg ocaml-base-compiler 5.4.1
  $ pkg ocaml-base-compiler 5.5.0

Two versions to choose between, a package that depends on either, and a package
with no dependencies at all.

  $ pkg b 1.0
  $ pkg b 2.0
  $ pkg a 1.0 'depends: [ "b" ]'

A package every version of which is one to avoid, and one that needs it.

  $ pkg avoided 1.0 'flags: [ avoid-version ]'
  $ pkg needs-avoided 1.0 'depends: [ "avoided" ]'

A package with a version to avoid alongside one to prefer.

  $ pkg mixed 1.0
  $ pkg mixed 2.0 'flags: [ avoid-version ]'
  $ pkg wants-mixed 1.0 'depends: [ "mixed" ]'

A base image already present, so no image is built: this exercises the solver
alone.  The platform is given rather than detected so the answers do not depend
on the machine.

  $ mkdir -p cache/debian-13-x86_64/base
  $ solve() {
  >   day10 health-check --dry-run --log --cache-dir cache --opam-repository repo \
  >     --os-distribution debian --os-version 13 --arch x86_64 "$@"
  > }

The newest version that satisfies the constraints.

  $ solve --ocaml-version ocaml.5.4.1 a.1.0
  digraph opam {
    "a.1.0" -> "b.2.0";
  }
  
  [NOTE] solution

The oldest instead, which is how a missing lower bound shows itself.

  $ solve --ocaml-version ocaml.5.4.1 --prefer-oldest a.1.0
  digraph opam {
    "a.1.0" -> "b.1.0";
  }
  
  [NOTE] solution

A version carrying avoid-version is left for one that does not.

  $ solve --ocaml-version ocaml.5.4.1 wants-mixed.1.0
  digraph opam {
    "wants-mixed.1.0" -> "mixed.1.0";
  }
  
  [NOTE] solution

But a package whose every version carries it is still installed rather than
called uninstallable, which is what opam does.  This needs the second pass.

  $ solve --ocaml-version ocaml.5.4.1 needs-avoided.1.0
  digraph opam {
    "needs-avoided.1.0" -> "avoided.1.0";
  }
  
  [NOTE] solution

A package that depends on nothing still appears, or a solution of one package
would be an empty graph.  ocaml is solved for as well, but does not show up
here: the solution holds what the requested package reaches, not everything the
solver resolved.

  $ solve --ocaml-version ocaml.5.4.1 b.1.0
  digraph opam {
    "b.1.0";
  }
  
  [NOTE] solution

Testing a compiler asks for one the requested ocaml version cannot admit, so
the invariant has to give way.  Without the flag there is no solution.

  $ solve --ocaml-version ocaml.5.4.1 ocaml-base-compiler.5.5.0
  Can't find all required versions.
  Selected: ocaml&ocaml-base-compiler
  - ocaml -> ocaml.5.4.1
      User requested = 5.4.1
  - ocaml-base-compiler -> (problem)
      User requested = 5.5.0
      ocaml 5.4.1 requires >= 5.4.1~ & < 5.4.2~
      Rejected candidates:
        ocaml-base-compiler.5.5.0: Incompatible with restriction: >= 5.4.1~ & < 5.4.2~
  [WARNING] no_solution

  $ solve --ocaml-version ocaml.5.4.1 --update-invariant ocaml-base-compiler.5.5.0
  digraph opam {
    "ocaml-base-compiler.5.5.0";
  }
  
  [NOTE] solution

Relaxing is a last resort: an ordinary package still resolves against the
version that was asked for, so passing the flag on every job is safe.

  $ solve --ocaml-version ocaml.5.4.1 --update-invariant a.1.0
  digraph opam {
    "a.1.0" -> "b.2.0";
  }
  
  [NOTE] solution

A post dependency is required but does not constrain build order.  It is
therefore not an ordering edge, and a package reached only through one does not
appear in the solution at all.

  $ pkg plain-post 1.0
  $ pkg needs-post 1.0 'depends: [ "b" "plain-post" {post} ]'

  $ solve --ocaml-version ocaml.5.4.1 needs-post.1.0
  digraph opam {
    "needs-post.1.0" -> "b.2.0";
  }
  
  [NOTE] solution

That exclusion is what lets a cycle through a post dependency resolve, which is
what post is for: without it the ordering has no starting point.

  $ pkg cyc-a 1.0 'depends: [ "cyc-b" ]'
  $ pkg cyc-b 1.0 'depends: [ "cyc-a" {post} ]'

  $ solve --ocaml-version ocaml.5.4.1 cyc-a.1.0
  digraph opam {
    "cyc-a.1.0" -> "cyc-b.1.0";
  }
  
  [NOTE] solution

But with-test trumps post.  A dependency that is only there because tests were
asked for has to be in the switch that runs them, so it is kept as an ordinary
dependency -- absent when tests are not requested, an ordering edge when they
are.  melange, ocamlformat, printbox-text and re write this.

  $ pkg tester 1.0
  $ pkg test-post 1.0 'depends: [ "b" "tester" {with-test & post} ]'

  $ solve --ocaml-version ocaml.5.4.1 test-post.1.0
  digraph opam {
    "test-post.1.0" -> "b.2.0";
  }
  
  [NOTE] solution

  $ solve --ocaml-version ocaml.5.4.1 --with-test test-post.1.0
  digraph opam {
    "test-post.1.0" -> {"b.2.0" "tester.1.0"}
  }
  
  [NOTE] solution

An optional dependency is an edge only when the solver included it anyway.  Here
one package reaches it through an ordinary dependency, so the package that
merely lists it as optional gains an edge to it too.

  $ pkg optional-extra 1.0
  $ pkg mid 1.0 'depends: [ "optional-extra" ]'
  $ pkg uses-depopt 1.0 'depends: [ "mid" ]' 'depopts: [ "optional-extra" ]'

  $ solve --ocaml-version ocaml.5.4.1 uses-depopt.1.0
  digraph opam {
    "mid.1.0" -> "optional-extra.1.0";
    "uses-depopt.1.0" -> {"mid.1.0" "optional-extra.1.0"}
  }
  
  [NOTE] solution

Nothing pulls it in here, so it stays out and the optional dependency is not an
edge at all.

  $ pkg ignores-depopt 1.0 'depends: [ "b" ]' 'depopts: [ "optional-extra" ]'

  $ solve --ocaml-version ocaml.5.4.1 ignores-depopt.1.0
  digraph opam {
    "ignores-depopt.1.0" -> "b.2.0";
  }
  
  [NOTE] solution

An ordinary test dependency appears only when tests are asked for, and then for
the package under test alone -- never for something it depends on.

  $ pkg only-for-tests 1.0
  $ pkg has-tests 1.0 'depends: [ "b" "only-for-tests" {with-test} ]'
  $ pkg needs-tested 1.0 'depends: [ "has-tests" ]'

  $ solve --ocaml-version ocaml.5.4.1 has-tests.1.0
  digraph opam {
    "has-tests.1.0" -> "b.2.0";
  }
  
  [NOTE] solution

  $ solve --ocaml-version ocaml.5.4.1 --with-test has-tests.1.0
  digraph opam {
    "has-tests.1.0" -> {"b.2.0" "only-for-tests.1.0"}
  }
  
  [NOTE] solution

  $ solve --ocaml-version ocaml.5.4.1 --with-test needs-tested.1.0
  digraph opam {
    "has-tests.1.0" -> "b.2.0";
    "needs-tested.1.0" -> "has-tests.1.0";
  }
  
  [NOTE] solution
