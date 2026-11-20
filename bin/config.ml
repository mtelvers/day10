type command = { run : string; network : bool }

type t = {
  dir : string;
  ocaml_version : OpamPackage.t;
  opam_repositories : string list;
  package : string;
  arch : string;
  os : string;
  os_distribution : string;
  os_family : string;
  os_version : string;
  directory : string option;
  md : string option;
  json : string option;
  dot : string option;
  with_test : bool;
  with_doc : bool;
  tag : string option;
  oci : string option;
  log : bool;
  dry_run : bool;
  fork : int option;
  (* What to run in the container instead of building a package.  A dune build
     has nothing to fetch, its dependencies being installed already, so it is
     given no network; a command the caller wrote keeps one, day10 having no way
     to tell what they meant by it. *)
  build_command : command option;
  (* How many jobs a package's build may run at once, or None for the default:
     one per core up to a ceiling.  Worth setting where a worker's slots and the
     machine's memory make the default wrong. *)
  opam_jobs : int option;
  (* The packages built from the workspace by dune, so never installed into the
     switch as well.  Defaults to every .opam file in the directory; a caller
     that has already worked out which packages it wants, as OCaml-CI has, names
     them with --only-packages instead. *)
  local_packages : string list;
  (* Solve for the lowest version of each dependency that the constraints allow
     rather than the highest, which is how a missing lower bound shows itself. *)
  prefer_oldest : bool;
  (* Let the solver choose the ocaml version when [ocaml_version] cannot be
     honoured, as opam's --update-invariant does for a switch.  Needed to test a
     compiler package, which determines the ocaml version itself. *)
  update_invariant : bool;
  (* Fetch every depext the repository names for the platform into a base's
     package cache as the base is built, rather than leave a build's depexts
     to its own start with the network.  Opt-in: some gigabytes per base. *)
  prefetch_base : bool;
}

let is_local_package ~(config : t) pkg =
  List.mem (OpamPackage.name_to_string pkg) config.local_packages

(* The package the caller asked about, as opposed to something it depends on.
   [package] is name.version for health-check and a bare name for ci, so it is
   compared as a package rather than by name. *)
let is_target_package ~(config : t) pkg =
  String.equal (OpamPackage.to_string pkg) config.package || is_local_package ~config pkg

(* The family opam will compute inside the container: the first word of ID_LIKE
   in its /etc/os-release, and the distribution where there is none
   (opamSysPoll.ml, poll_os_family).  Derived here rather than polled from the
   builder, which describes the builder: an alpine job on an Ubuntu worker was
   given family debian, so every depext line keyed on the debian family matched,
   and packages that were never installed went into the layer key.

   Only the distributions whose ID_LIKE says something other than their own name
   need listing.  opensuse splits on the version because Leap and Tumbleweed
   disagree -- "suse opensuse" against "opensuse suse" -- and day10 calls them
   both opensuse. *)
let family ~given ~distribution ~version =
  match given with
  | Some family -> family
  | None -> (
      match (distribution, version) with
      | ("debian" | "ubuntu"), _ -> "debian"
      | ("centos" | "rhel" | "ol" | "almalinux" | "rocky"), _ -> "rhel"
      | "opensuse", "tumbleweed" -> "opensuse"
      | "opensuse", _ -> "suse"
      | ("archlinux" | "arch"), _ -> "arch"
      (* opam says windows for every flavour of it (opamSysPoll.ml,
         poll_os_family), and 81 depext and availability filters in the
         repository are written against that. *)
      | ("win32" | "cygwin" | "msys2"), _ -> "windows"
      | ("freebsd" | "openbsd" | "netbsd" | "dragonfly"), _ -> "bsd"
      | distribution, _ -> distribution)

(* The distribution opam will compute inside the container: the ID field of its
   /etc/os-release (opamSysPoll.ml, os_distribution).  The names the CI and the
   cache use for a platform are not always that: the Arch image says arch, and
   the two openSUSE images say opensuse-leap and opensuse-tumbleweed where day10
   calls both opensuse.  A depext filter written against opam's value has to be
   evaluated against opam's value, or the layer key and the base's prefetch
   leave out what opam then installs. *)
let distribution ~distribution ~version =
  match (distribution, version) with
  | ("archlinux" | "arch"), _ -> "arch"
  | "opensuse", "tumbleweed" -> "opensuse-tumbleweed"
  | "opensuse", _ -> "opensuse-leap"
  | distribution, _ -> distribution

(* The version to build when none was given.  The host's version is only an
   answer when the distribution is the host's too; otherwise an Ubuntu worker
   would label every other distribution 26.04.  Arch has no version, so its
   image is tagged latest.  cygwin and msys2 are Windows with a Unix layer
   chosen, so a Windows host's version is theirs.  None asks the caller to
   say. *)
let version ~host:(host_distribution, host_version) ~distribution = function
  | Some version -> Some version
  | None -> (
      match distribution with
      | "archlinux" | "arch" -> Some "latest"
      | _ when String.equal distribution host_distribution -> Some host_version
      | "cygwin" | "msys2" when String.equal host_distribution "win32" -> Some host_version
      | _ -> None)

let std_env ~(config : t) =
  Util.std_env ~arch:config.arch ~os:config.os
    ~os_distribution:(distribution ~distribution:config.os_distribution ~version:config.os_version)
    ~os_family:config.os_family ~os_version:config.os_version ~ocaml_version:config.ocaml_version ()

let platform_vars ~(config : t) =
  Util.platform_vars ~arch:config.arch ~os:config.os
    ~os_distribution:(distribution ~distribution:config.os_distribution ~version:config.os_version)
    ~os_family:config.os_family ~os_version:config.os_version

(* The platform as x-ci-accept-failures names it: distribution and version,
   without the architecture that os_key carries. *)
let platform ~(config : t) = config.os_distribution ^ "-" ^ config.os_version

(* The names an x-ci-accept-failures entry may give this platform.  The
   entries are written against opam-repo-ci's platform names, which are what
   its OBuilder jobs grep for, and those differ from day10's own where day10
   keeps opam's values:
   - Windows: cygwin-mingw / msys2-mingw; bare cygwin is pure Cygwin, which
     day10 does not build.
   - FreeBSD: freebsd-15.1, where opam's os-version is the osreldate 1501000.
   - macOS: macos-homebrew, where day10's distribution is homebrew.
   The bare distribution matches as well. *)
let accept_failure_keys ~(config : t) =
  if String.equal config.os_family "windows" then [ config.os_distribution ^ "-mingw" ]
  else
    let ci =
      match (config.os_distribution, int_of_string_opt config.os_version) with
      | "freebsd", Some v when v >= 1000000 -> [ Printf.sprintf "freebsd-%d.%d" (v / 100000) (v / 1000 mod 100) ]
      | "homebrew", _ -> [ "macos-homebrew" ]
      | _ -> []
    in
    ci @ [ platform ~config; config.os_distribution ]

(* The platform directory in the cache, named as the CI names the platform
   rather than as opam reports it, so that the openSUSE directories keep their
   names. *)
let os_key ~(config : t) = String.concat "-" [ config.os_distribution; config.os_version; config.arch ]
