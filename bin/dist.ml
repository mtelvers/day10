open Dockerfile

(* Not "open Dockerfile_opam": it exports Linux and Windows modules of its own,
   which would shadow day10's. *)
module Distro = Dockerfile_opam.Distro

(* The distribution-specific parts of a base image: how to install packages,
   what the packages are called, and how to create the opam user.  Everything
   else about the image is the same everywhere and lives in Dockerfile_gen. *)
type t = {
  update : string;  (** shell command refreshing the package index *)
  upgrade : string;  (** shell command upgrading the installed packages *)
  install : string -> string;  (** shell command installing the given packages *)
  deps_opam : string;  (** needed to build opam from source *)
  deps_opam_build : string;  (** needed to build opam-build from source *)
  deps_runtime : string;  (** needed in the final image *)
  noninteractive : Dockerfile.t;  (** stop the package manager prompting *)
  add_user : uid:int -> gid:int -> Dockerfile.t;
}

let sudoers =
  run "echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam"
  @@ run "chmod 440 /etc/sudoers.d/opam"
  @@ run "chown root:root /etc/sudoers.d/opam"

(* The uid and gid have to come out as the values day10 asks for: bin/linux.ml
   runs the container as 1000:1000 and chowns the bind-mounted source to match,
   so a user landing on a different id would leave the build unable to write to
   its own source directory.  Create the group explicitly rather than letting
   useradd pick the next free gid, and free the id first if the base image ships
   a user already holding it (Ubuntu, for one, does). *)
let useradd ~uid ~gid =
  run "if getent passwd %i; then userdel -r $(id -nu %i); fi" uid uid
  @@ run "groupadd --gid %i opam" gid
  @@ run "useradd --uid %i --gid %i --home-dir /home/opam --create-home --shell /bin/bash opam" uid gid
  @@ run "chown -R %i:%i /home/opam" uid gid
  @@ sudoers

let apt =
  {
    update = "apt update";
    upgrade = "apt upgrade -y";
    install = (fun packages -> "apt install -y " ^ packages);
    deps_opam = "build-essential git curl libcap-dev sudo";
    deps_opam_build = "build-essential git curl unzip bubblewrap";
    deps_runtime = "build-essential unzip bubblewrap git sudo curl rsync";
    noninteractive = run "echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections";
    add_user =
      (fun ~uid ~gid ->
        run "if getent passwd %i; then userdel -r $(id -nu %i); fi" uid uid
        @@ run "groupadd --gid %i opam" gid
        @@ run "adduser --disabled-password --gecos '@opam' --no-create-home --uid %i --gid %i --home /home/opam opam" uid gid
        @@ run "mkdir -p /home/opam && chown -R %i:%i /home/opam" uid gid
        @@ sudoers);
  }

let yum =
  {
    update = "yum makecache";
    upgrade = "yum update -y";
    install = (fun packages -> "yum install -y " ^ packages);
    deps_opam = "gcc make patch unzip bzip2 tar git curl sudo diffutils findutils libcap-devel";
    deps_opam_build = "gcc make patch unzip bzip2 tar git curl diffutils findutils bubblewrap";
    deps_runtime = "gcc gcc-c++ make patch unzip bzip2 tar xz git curl sudo rsync diffutils findutils m4 gawk which bubblewrap";
    noninteractive = empty;
    add_user = useradd;
  }

let apk =
  {
    update = "apk update";
    upgrade = "apk upgrade";
    install = (fun packages -> "apk add " ^ packages);
    deps_opam = "build-base patch unzip bzip2 tar git curl sudo linux-headers libcap-dev";
    deps_opam_build = "build-base patch unzip bzip2 tar git curl bubblewrap";
    deps_runtime = "build-base patch unzip bzip2 tar xz git curl sudo rsync bash coreutils diffutils bubblewrap";
    noninteractive = empty;
    (* Alpine ships busybox's adduser rather than shadow's useradd, and its
       flags are not the same. *)
    add_user =
      (fun ~uid ~gid ->
        run "addgroup -g %i opam" gid
        @@ run "adduser -D -u %i -G opam -h /home/opam -s /bin/sh opam" uid
        @@ run "chown -R %i:%i /home/opam" uid gid
        @@ sudoers);
  }

let zypper =
  {
    update = "zypper refresh";
    upgrade = "zypper update -y";
    install = (fun packages -> "zypper install -y " ^ packages);
    deps_opam = "gcc make patch unzip bzip2 tar git curl sudo diffutils findutils libcap-devel gzip";
    deps_opam_build = "gcc make patch unzip bzip2 tar git curl diffutils findutils gzip";
    deps_runtime = "gcc gcc-c++ make patch unzip bzip2 tar xz git curl sudo rsync diffutils findutils m4 gawk which gzip";
    noninteractive = empty;
    add_user = useradd;
  }

let pacman =
  {
    update = "pacman -Sy --noconfirm";
    upgrade = "pacman -Su --noconfirm";
    install = (fun packages -> "pacman -S --noconfirm --needed " ^ packages);
    deps_opam = "gcc make patch unzip bzip2 tar git curl sudo diffutils libcap";
    deps_opam_build = "gcc make patch unzip bzip2 tar git curl diffutils bubblewrap";
    deps_runtime = "gcc make patch unzip bzip2 tar xz git curl sudo rsync diffutils which bubblewrap";
    noninteractive = empty;
    add_user = useradd;
  }

(* ocaml-dockerfile knows the real Docker Hub image names, which are not always
   "distribution:version" -- openSUSE lives under opensuse/leap, Arch is rolling
   so has no version, and i386 and arm32 need their own image prefixes.  It is
   only consulted as a lookup table: anything it does not recognise, such as a
   distribution released after the version of ocaml-dockerfile we are built
   against, still works the way it did before. *)
let distro_of ~distribution ~version =
  match Distro.distro_of_tag (distribution ^ "-" ^ version) with
  | Some _ as distro -> distro
  | None -> Distro.distro_of_tag distribution

let of_package_manager = function
  | `Apt -> Some apt
  | `Yum -> Some yum
  | `Apk -> Some apk
  | `Zypper -> Some zypper
  | `Pacman -> Some pacman
  | `Cygwin | `Windows -> None

let of_os_family = function
  | "debian" -> Some apt
  | "fedora" | "rhel" | "centos" | "ol" | "amzn" -> Some yum
  | "alpine" -> Some apk
  | "suse" | "opensuse" | "sles" -> Some zypper
  | "arch" | "archlinux" -> Some pacman
  | _ -> None

let of_config ~os_family ~distribution ~version =
  match distro_of ~distribution ~version with
  | Some distro -> of_package_manager (Distro.package_manager distro)
  | None -> of_os_family os_family

let base_image ~arch ~distribution ~version =
  match distro_of ~distribution ~version with
  | Some distro ->
      let image, tag =
        match Ocaml_version.of_opam_arch arch with
        | Some arch -> Distro.base_distro_tag ~arch distro
        | None -> Distro.base_distro_tag distro
      in
      image ^ ":" ^ tag
  | None -> Printf.sprintf "%s:%s" distribution version
