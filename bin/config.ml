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
      | distribution, _ -> distribution)

let std_env ~(config : t) =
  Util.std_env ~arch:config.arch ~os:config.os ~os_distribution:config.os_distribution ~os_family:config.os_family ~os_version:config.os_version
    ~ocaml_version:config.ocaml_version ()

let platform_vars ~(config : t) =
  Util.platform_vars ~arch:config.arch ~os:config.os ~os_distribution:config.os_distribution ~os_family:config.os_family ~os_version:config.os_version

(* The platform as x-ci-accept-failures names it: distribution and version,
   without the architecture that os_key carries. *)
let platform ~(config : t) = config.os_distribution ^ "-" ^ config.os_version

let os_key ~(config : t) =
  let os =
    List.map
      (fun v -> std_env ~config v |> Option.map OpamVariable.string_of_variable_contents |> Option.value ~default:"unknown")
      [ "os-distribution"; "os-version"; "arch" ]
  in
  String.concat "-" os
