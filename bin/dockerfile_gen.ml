open Dockerfile

let platform = function
  | "x86_64" | "amd64" -> "linux/amd64"
  | "i386" | "i486" | "i586" | "i686" -> "linux/386"
  | "aarch64" -> "linux/arm64"
  | "armv7l" -> "linux/arm/v7"
  | "armv6l" -> "linux/arm/v6"
  | "ppc64le" -> "linux/ppc64le"
  | "riscv64" -> "linux/riscv64"
  | "s390x" -> "linux/s390x"
  | arch -> "linux/" ^ arch

let opam ~(dist : Dist.t) ~arch base_image =
  from ~platform:(platform arch) ~alias:"opam-builder" base_image
  @@ run "%s && %s" dist.update (dist.install dist.deps_opam)
  @@ run "git clone --depth 1 --branch 2.4.1 https://github.com/ocaml/opam.git /tmp/opam"
  @@ workdir "/tmp/opam"
  @@ run "make cold"
  @@ run "make install"

let opam_build ~(dist : Dist.t) ~arch base_image =
  from ~platform:(platform arch) ~alias:"opam-build-builder" base_image
  @@ run "%s && %s" dist.update (dist.install dist.deps_opam_build)
  @@ copy ~from:"opam-builder" ~src:[ "/usr/local/bin/opam" ] ~dst:"/usr/local/bin/opam" ()
  @@ run "opam init --disable-sandboxing -a --bare -y"
  @@ run "git clone --depth 1 --branch master https://github.com/mtelvers/opam-build.git /tmp/opam-build"
  @@ workdir "/tmp/opam-build"
  @@ run "opam switch create . 5.3.0 --deps-only -y"
  @@ run "opam exec -- dune build --release"
  @@ run "install -m 755 _build/default/bin/main.exe /usr/local/bin/opam-build"

let dockerfile ~(dist : Dist.t) ~arch ~base_image ~uid ~gid =
  (opam ~dist ~arch base_image) @@ (opam_build ~dist ~arch base_image)
  @@ from ~platform:(platform arch) base_image
  (* One RUN, so the package index cannot be served from a stale cached layer
     while the mirror has moved on -- that combination 404s on every package. *)
  @@ run "%s && %s && %s" dist.update dist.upgrade (dist.install dist.deps_runtime)
  @@ copy ~from:"opam-builder" ~src:[ "/usr/local/bin/opam" ] ~dst:"/usr/local/bin/opam" ()
  @@ copy ~from:"opam-build-builder" ~src:[ "/usr/local/bin/opam-build" ] ~dst:"/usr/local/bin/opam-build" ()
  @@ dist.noninteractive
  @@ dist.add_user ~uid ~gid
  @@ copy ~chown:(string_of_int uid ^ ":" ^ string_of_int gid) ~src:[ "opam-repository" ] ~dst:"/home/opam/opam-repository" ()
  @@ user "%i:%i" uid gid @@ workdir "/home/opam"
  @@ run "opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y"
  @@ run "opam switch create default --empty"
