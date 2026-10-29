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
  enable_repos : string option;  (** shell command turning on repositories the image ships disabled *)
  deps_opam : string;  (** needed to build opam from source *)
  deps_day10_install : string;  (** needed to build day10-install from source *)
  deps_runtime : string;  (** needed in the final image *)
  keep_downloads : string list;  (** shell commands making the package manager keep what it downloads and never refresh its index *)
  download : string -> string;  (** shell command fetching the given packages into the download cache without installing them *)
  noninteractive : Dockerfile.t;  (** stop the package manager prompting *)
  add_user : uid:int -> gid:int -> Dockerfile.t;
}

(* Enabling a repository has to come before the index is refreshed, and the
   refresh has to stay in the same command as the install that depends on it. *)
let shell dist commands = String.concat " && " (Option.to_list dist.enable_repos @ commands)

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
    enable_repos = None;
    deps_opam = "build-essential git curl libcap-dev sudo";
    deps_day10_install = "build-essential git curl unzip bubblewrap";
    deps_runtime = "build-essential unzip bubblewrap git sudo curl rsync";
    (* The Docker images hook apt-get clean onto every dpkg run, which would
       empty the cache in the base and, worse, in every layer that installs a
       depext, hiding it from the layers above.  apt-get does not otherwise
       drop a download, and install consults no expiry. *)
    keep_downloads = [ "rm -f /etc/apt/apt.conf.d/docker-clean" ];
    download = (fun packages -> "apt-get install -y --download-only " ^ packages);
    noninteractive = run "echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections";
    add_user = useradd;
  }

let yum =
  {
    update = "yum makecache";
    upgrade = "yum update -y";
    (* --allowerasing so a package may replace one the image already has:
       RHEL 9 ships curl-minimal, which conflicts with curl. *)
    install = (fun packages -> "yum install -y --allowerasing " ^ packages);
    (* Set by of_config, which knows the version. *)
    enable_repos = None;
    deps_opam = "gcc gcc-c++ make patch unzip bzip2 tar git curl openssl sudo diffutils findutils libcap-devel";
    deps_day10_install = "gcc gcc-c++ make patch unzip bzip2 tar git curl diffutils findutils bubblewrap";
    deps_runtime = "gcc gcc-c++ make patch unzip bzip2 tar xz git curl openssl sudo rsync diffutils findutils m4 gawk which bubblewrap";
    (* dnf discards downloads unless told to keep them, and refuses to install
       once its metadata is older than the repository's metadata_expire, which
       every stock .repo file sets, so that is overridden in each of them as
       well as in the defaults. *)
    keep_downloads =
      [ "printf 'keepcache=True\\nmetadata_expire=never\\n' >> /etc/dnf/dnf.conf"; "sed -i 's/^metadata_expire=.*/metadata_expire=never/' /etc/yum.repos.d/*.repo" ];
    download = (fun packages -> "yum install -y --downloadonly " ^ packages);
    noninteractive = empty;
    add_user = useradd;
  }

let apk =
  {
    update = "apk update";
    upgrade = "apk upgrade";
    install = (fun packages -> "apk add " ^ packages);
    enable_repos = None;
    deps_opam = "build-base patch unzip bzip2 tar git curl openssl sudo linux-headers libcap-dev";
    deps_day10_install = "build-base patch unzip bzip2 tar git curl bubblewrap";
    deps_runtime = "build-base patch unzip bzip2 tar xz git curl sudo rsync bash coreutils diffutils bubblewrap";
    (* apk caches only when /etc/apk/cache exists.  It has no download-only
       verb and its cache names a file by content hash, so the way to fill the
       cache is to install into a throwaway root that shares it. *)
    keep_downloads = [ "ln -sf /var/cache/apk /etc/apk/cache" ];
    download =
      (fun packages ->
        "rm -rf /tmp/prefetch && apk add --root /tmp/prefetch --initdb --keys-dir /etc/apk/keys --repositories-file /etc/apk/repositories --cache-dir /etc/apk/cache "
        ^ packages);
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
    enable_repos = None;
    deps_opam = "gcc gcc-c++ make patch unzip bzip2 tar git curl sudo diffutils findutils libcap-devel gzip";
    deps_day10_install = "gcc gcc-c++ make patch unzip bzip2 tar git curl diffutils findutils gzip";
    deps_runtime = "gcc gcc-c++ make patch unzip bzip2 tar xz git curl openssl sudo rsync diffutils findutils m4 gawk which gzip";
    (* zypper skips a repository whose refresh fails, so autorefresh has to go,
       on the service that manages the repositories as well as on them, since a
       service refresh would put it back. *)
    keep_downloads = [ "zypper -n mr -k --all"; "zypper -n ms -R --all"; "zypper -n mr -R --all" ];
    download = (fun packages -> "zypper -n install --download-only " ^ packages);
    noninteractive = empty;
    add_user = useradd;
  }

let pacman =
  {
    update = "pacman -Sy --noconfirm";
    upgrade = "pacman -Su --noconfirm";
    install = (fun packages -> "pacman -S --noconfirm --needed " ^ packages);
    enable_repos = None;
    deps_opam = "gcc make patch unzip bzip2 tar git curl sudo diffutils libcap";
    deps_day10_install = "gcc make patch unzip bzip2 tar git curl diffutils bubblewrap";
    deps_runtime = "gcc make patch unzip bzip2 tar xz git curl sudo rsync diffutils which bubblewrap";
    (* pacman keeps every download and refreshes only when asked with -y. *)
    keep_downloads = [];
    download = (fun packages -> "pacman -Sw --noconfirm " ^ packages);
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

(* CentOS keeps many of the -devel and -static packages opam names as depexts in
   CodeReady Builder, and ships it disabled, so installing them reports no such
   package.  It was called powertools on 8 and crb from 9 on.  Another 59 of the
   names live only in EPEL, which is enabled by installing epel-release.  opam
   does that itself when a package's depexts name epel-release, but a layer
   holds one package's depexts, and 28 packages name an EPEL package without
   naming epel-release, relying on another package in the same install to
   bring it.  Nothing else in the matrix holds a repository back this way. *)
let codeready_builder : Distro.t option -> string option = function
  | Some (`CentOS (`V6 | `V7)) -> None
  | Some (`CentOS `V8) -> Some "dnf config-manager --set-enabled powertools && dnf install -y epel-release"
  | Some (`CentOS _) -> Some "dnf config-manager --set-enabled crb && dnf install -y epel-release"
  | Some _ | None -> None

let of_config ~os_family ~distribution ~version =
  let distro = distro_of ~distribution ~version in
  let dist =
    match distro with
    | Some distro -> of_package_manager (Distro.package_manager distro)
    | None -> of_os_family os_family
  in
  Option.map (fun dist -> { dist with enable_repos = codeready_builder distro }) dist

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
