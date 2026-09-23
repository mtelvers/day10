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

A build gets one job per core up to a ceiling.  The container sees every core
the machine has however many other containers are running, so on a 256-core
worker at 64 jobs a package building with "make -j jobs" asked for 255 apiece:
z3 holds about 375M per cc1plus, which is six terabytes, and the machine died.
The count is only ever wrong upwards -- a four-core board already comes out at
three -- so it is capped rather than replaced.

  $ day10 exec project -- sh -c 'cores=$(nproc); want=$((cores - 1)); [ $want -gt 32 ] && want=32; [ $want -lt 1 ] && want=1; [ "$(opam var jobs)" = "$want" ] && echo "jobs matches the cap"'
  jobs matches the cap

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

Tests run in a container of their own, with a network namespace of their own.
A builder runs dozens of jobs at once against one localhost, so two suites
binding the same fixed port would otherwise collide -- or worse, one would
reach the other's server and report on it.  Taking the network away also makes
a suite that downloads something fail here, as it does under OBuilder, rather
than passing on a machine that happens to have a route out.

The package under test installs first, with the network, so that the run that
has none still finds its sources and its depexts in place.

  $ mkdir -p repo/packages/netprobe/netprobe.1.0
  $ cat > repo/packages/netprobe/netprobe.1.0/opam <<'EOF'
  > opam-version: "2.0"
  > build: [ "sh" "-c" "curl -s -m 10 -o /dev/null https://opam.ocaml.org/index.tar.gz && echo BUILD-ONLINE || echo BUILD-OFFLINE" ]
  > run-test: [ "sh" "-c" "curl -s -m 10 -o /dev/null https://opam.ocaml.org/index.tar.gz && echo TEST-ONLINE || echo TEST-OFFLINE" ]
  > EOF

  $ NET=$(mktemp -d)
  $ mkdir -p "$NET/$OSKEY"
  $ sudo cp -al "$REAL/$OSKEY/base" "$NET/$OSKEY/base"
  $ day10 health-check --with-test --log --cache-dir "$NET" --opam-repository repo netprobe.1.0 > net.log 2>&1
  $ grep -c '^- BUILD-ONLINE$' net.log
  1
  $ grep -c '^- TEST-OFFLINE$' net.log
  1

Both runs are in the one log.  The second used to open it truncating, so the
install's output -- the half that has the network, and so the half where a
download fails -- was thrown away before anyone could read it.

  $ grep -c '^- BUILD-OFFLINE$' net.log
  1

  $ sudo rm -rf "$NET"

A command the caller wrote keeps the network: day10 cannot tell what they meant
by it, and it is their own machine running the one job.  A dune build is day10's
own command and has nothing to fetch -- the dependencies are installed in the
layers beneath it and the sources are bind mounted -- so it is given none, and a
project that turns out to need it finds that out here rather than in CI.

  $ mkdir -p netproject
  $ cat > netproject/probe.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [ "dune" ]
  > EOF
  $ cat > netproject/dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF
  $ cat > netproject/dune <<'EOF'
  > (rule
  >  (alias netprobe)
  >  (action
  >   (run sh -c "curl -s -m 10 -o /dev/null https://opam.ocaml.org/index.tar.gz && echo ONLINE || echo OFFLINE")))
  > EOF

  $ day10 exec netproject -- sh -c 'curl -s -m 10 -o /dev/null https://opam.ocaml.org/index.tar.gz && echo ONLINE || echo OFFLINE'
  ONLINE

  $ day10 build netproject @netprobe
  OFFLINE

  $ sudo rm -rf "$CACHE"
