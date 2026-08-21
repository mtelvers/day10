open Dockerfile

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
   adduser pick the next free gid, and free the id first if the base image ships
   a user already holding it (Ubuntu, for one, does). *)
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
