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
