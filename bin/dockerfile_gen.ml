open Dockerfile

(* Three vocabularies meet here.  A caller passes opam's arch names -- that is
   what opam-repo-ci normalises to before submitting -- or the uname ones if a
   person typed it, and Docker wants a third set again.  Take both spellings:
   the fallthrough is right only where the two happen to coincide, which is
   arm64, s390x and riscv64 but not ppc64, x86_32 or arm32.  linux/ppc64 names
   the big-endian platform, which no image publishes a manifest for. *)
let platform = function
  | "x86_64" | "amd64" -> "linux/amd64"
  | "x86_32" | "i386" | "i486" | "i586" | "i686" -> "linux/386"
  | "arm64" | "aarch64" -> "linux/arm64"
  | "arm32" | "armv7l" -> "linux/arm/v7"
  | "armv6l" -> "linux/arm/v6"
  | "ppc64" | "ppc64le" -> "linux/ppc64le"
  | "riscv64" -> "linux/riscv64"
  | "s390x" -> "linux/s390x"
  | arch -> "linux/" ^ arch

let opam ~(dist : Dist.t) ~arch base_image =
  from ~platform:(platform arch) ~alias:"opam-builder" base_image
  @@ run "%s" (Dist.shell dist [ dist.update; dist.install dist.deps_opam ])
  @@ run "git clone --depth 1 --branch 2.4.1 https://github.com/ocaml/opam.git /tmp/opam"
  @@ workdir "/tmp/opam"
  @@ run "make cold"
  @@ run "make install"

let day10_install ~(dist : Dist.t) ~arch base_image =
  from ~platform:(platform arch) ~alias:"day10-install-builder" base_image
  @@ run "%s" (Dist.shell dist [ dist.update; dist.install dist.deps_day10_install ])
  @@ copy ~from:"opam-builder" ~src:[ "/usr/local/bin/opam" ] ~dst:"/usr/local/bin/opam" ()
  @@ run "opam init --disable-sandboxing -a --bare -y"
  (* Docker keys a RUN on its text, so cloning master is cached forever and the
     image keeps whichever day10-install it was first built with.  A remote ADD is
     fetched and checksummed on every build, so naming the ref here rebuilds
     from the clone down whenever master moves. *)
  @@ add ~src:[ "https://api.github.com/repos/mtelvers/day10-install/git/refs/heads/master" ] ~dst:"/tmp/day10-install.ref" ()
  @@ run "git clone --depth 1 --branch master https://github.com/mtelvers/day10-install.git /tmp/day10-install"
  @@ workdir "/tmp/day10-install"
  @@ run "opam switch create . 5.3.0 --deps-only -y"
  @@ run "opam exec -- dune build --release"
  @@ run "install -m 755 _build/default/bin/main.exe /usr/local/bin/day10-install"

(* Fill the package manager's download cache with every depext the repository
   names for this platform, so that a build installs from the base and never
   from a mirror that has since moved on.  One line per distinct depext set,
   resolved on its own since the union of them all is not co-installable; a
   set that fails as a whole, one of its names being unknown here, is retried a
   package at a time so the rest are still fetched. *)
let prefetch ~(dist : Dist.t) =
  Printf.sprintf {|while read -r s; do [ -n "$s" ] || continue; %s || for p in $s; do %s || true; done; done < /tmp/depexts && rm -rf /tmp/depexts /tmp/prefetch|}
    (dist.download "$s") (dist.download "$p")

let dockerfile ~(dist : Dist.t) ~arch ~base_image ~uid ~gid =
  (opam ~dist ~arch base_image) @@ (day10_install ~dist ~arch base_image)
  @@ from ~platform:(platform arch) base_image
  @@ copy ~src:[ "depexts" ] ~dst:"/tmp/depexts" ()
  (* One RUN, so the index, the system upgraded to it and the packages
     fetched against it are from the same moment: served from a stale cached
     layer while the mirror has moved on, any one of them apart from the others
     404s on every package.  The index is never refreshed after this. *)
  @@ run "%s" (Dist.shell dist ([ dist.update; dist.upgrade; dist.install dist.deps_runtime ] @ dist.keep_downloads @ [ prefetch ~dist ]))
  @@ copy ~from:"opam-builder" ~src:[ "/usr/local/bin/opam" ] ~dst:"/usr/local/bin/opam" ()
  @@ copy ~from:"day10-install-builder" ~src:[ "/usr/local/bin/day10-install" ] ~dst:"/usr/local/bin/day10-install" ()
  @@ dist.noninteractive
  @@ dist.add_user ~uid ~gid
  @@ copy ~chown:(string_of_int uid ^ ":" ^ string_of_int gid) ~src:[ "opam-repository" ] ~dst:"/home/opam/opam-repository" ()
  @@ user "%i:%i" uid gid @@ workdir "/home/opam"
  @@ run "opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y"
  (* Fall back to opam's cache when a package's upstream sources are gone. *)
  @@ run "%s" {|echo 'archive-mirrors: "https://opam.ocaml.org/cache"' >> /home/opam/.opam/config|}
  @@ run "opam switch create default --empty"
