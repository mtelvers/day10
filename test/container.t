These need a container, so they are not part of runtest: they want runc, sudo,
a base image and a warm cache, none of which a plain checkout has.  Run them on
a builder with "dune build @container".

They read the machine's own cache and opam-repository from ~/.day10 rather than
setting HOME aside as the other cram tests do, because a base image is far too
expensive to build for a test.

A project of one package, with nothing to build, so what is being exercised is
the container rather than the compiler.

  $ mkdir -p project
  $ cat > project/probe.opam <<'EOF'
  > opam-version: "2.0"
  > EOF
  $ cat > project/dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF

/tmp is a tmpfs in the container and was mounted noexec, so a build or test
suite that staged a binary there and ran it failed with a bare permission
denied naming neither /tmp nor the mount.

  $ day10 exec project -- sh -c 'cp /bin/true /tmp/staged && /tmp/staged && echo EXEC-OK'
  EXEC-OK

It is still a tmpfs, so what a build leaves there stays out of the cached layer.

  $ day10 exec project -- sh -c 'mount | grep -c " /tmp "'
  1

The build runs as opam, not as root.

  $ day10 exec project -- sh -c 'id -un'
  opam

The rest need a build that fails, so they run against a cache of their own and
leave nothing in the machine's.  The base image is hardlinked in rather than
built: it takes minutes, and is the one thing here worth reusing.  It lives
outside the sandbox because its files are root-owned and dune could not clean
up after them.

  $ REAL=$(grep '^CACHE_DIR=' "$HOME/.day10" | cut -d= -f2)
  $ OSKEY=$(ls "$REAL" | grep -v '^temp-' | head -1)
  $ CACHE=$(mktemp -d)
  $ mkdir -p "$CACHE/$OSKEY"
  $ sudo cp -al "$REAL/$OSKEY/base" "$CACHE/$OSKEY/base"

A repository of its own, holding one package that fails and is declared as
expected to fail on this platform.  The platform is the cache directory's name
without the architecture.

  $ mkdir -p repo/packages/ocaml/ocaml.5.4.1 repo/packages/boom/boom.1.0
  $ printf 'opam-version: "2.0"\n' > repo/repo
  $ printf 'opam-version: "2.0"\n' > repo/packages/ocaml/ocaml.5.4.1/opam
  $ cat > repo/packages/boom/boom.1.0/opam <<EOF
  > opam-version: "2.0"
  > build: [ "sh" "-c" "echo THE-REAL-ERROR; exit 1" ]
  > x-ci-accept-failures: [ "${OSKEY%-*}" ]
  > EOF

  $ day10 health-check --cache-dir "$CACHE" --opam-repository repo boom.1.0 > first.log 2>&1

The log is the whole explanation of a failure, so it has to be there.

  $ grep -q THE-REAL-ERROR first.log && echo present
  present

And the maintainer's declaration that a failure here is expected is reported,
so that whatever reads this can tell it from a regression.

  $ grep -c '^\[NOTE\] accept_failures$' first.log
  1

Asking again answers from the cached layer without rebuilding, and still says
both things: a verdict with no log is no use to the caller whether the failure
happened just now or weeks ago.

  $ day10 health-check --cache-dir "$CACHE" --opam-repository repo boom.1.0 > second.log 2>&1
  $ grep -c 'Building boom' second.log
  0
  [1]
  $ grep -q THE-REAL-ERROR second.log && echo still present
  still present
  $ grep -c '^\[NOTE\] accept_failures$' second.log
  1

  $ sudo rm -rf "$CACHE"
