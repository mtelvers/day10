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

let opam ~arch base_image =
  from ~platform:(platform arch) ~alias:"opam-builder" base_image
  @@ run "apt update && apt install -y build-essential git curl libcap-dev sudo"
  @@ run "git clone --depth 1 --branch 2.4.1 https://github.com/ocaml/opam.git /tmp/opam"
  @@ workdir "/tmp/opam"
  @@ run "make cold"
  @@ run "make install"

let opam_build ~arch base_image =
  from ~platform:(platform arch) ~alias:"opam-build-builder" base_image
  @@ run "apt update && apt install -y build-essential git curl unzip bubblewrap"
  @@ copy ~from:"opam-builder" ~src:[ "/usr/local/bin/opam" ] ~dst:"/usr/local/bin/opam" ()
  @@ run "opam init --disable-sandboxing -a --bare -y"
  @@ run "git clone --depth 1 --branch master https://github.com/mtelvers/opam-build.git /tmp/opam-build"
  @@ workdir "/tmp/opam-build"
  @@ run "opam switch create . 5.3.0 --deps-only -y"
  @@ run "opam exec -- dune build --release"
  @@ run "install -m 755 _build/default/bin/main.exe /usr/local/bin/opam-build"

let debian ~arch ~distribution ~version ~uid ~gid =
  let base_image = Printf.sprintf "%s:%s" distribution version in
  (opam ~arch base_image) @@ (opam_build ~arch base_image)
  @@ from ~platform:(platform arch) base_image
  @@ run "apt update && apt upgrade -y"
  @@ run "apt install build-essential unzip bubblewrap git sudo curl rsync -y"
  @@ copy ~from:"opam-builder" ~src:[ "/usr/local/bin/opam" ] ~dst:"/usr/local/bin/opam" ()
  @@ copy ~from:"opam-build-builder" ~src:[ "/usr/local/bin/opam-build" ] ~dst:"/usr/local/bin/opam-build" ()
  @@ run "echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections"
  @@ run "if getent passwd %i; then userdel -r $(id -nu %i); fi" uid uid
  @@ run "groupadd --gid %i opam" gid
  @@ run "adduser --disabled-password --gecos '@opam' --no-create-home --uid %i --gid %i --home /home/opam opam" uid gid
  @@ run "mkdir -p /home/opam && chown -R %i:%i /home/opam" uid gid
  @@ run "echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam"
  @@ run "chmod 440 /etc/sudoers.d/opam" @@ run "chown root:root /etc/sudoers.d/opam"
  @@ copy ~chown:(string_of_int uid ^ ":" ^ string_of_int gid) ~src:[ "opam-repository" ] ~dst:"/home/opam/opam-repository" ()
  @@ user "%i:%i" uid gid @@ workdir "/home/opam"
  @@ run "opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y"
  @@ run "opam switch create default --empty"
