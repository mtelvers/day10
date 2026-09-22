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
  (* What to run in the container instead of building a package, and whether it
     may reach the network.  A dune build has nothing to fetch -- the
     dependencies are installed in the layers under it and the sources are bind
     mounted -- so it is given none, and a project that turns out to need it
     finds that out here rather than in CI.  A command the caller wrote is
     another matter: day10 cannot tell what they meant by it, so it keeps the
     network it would have had outside the container. *)
  build_command : command option;
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
