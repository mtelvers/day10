type rejection =
  | UserConstraint of OpamFormula.atom
  | Unavailable

let ( / ) = Filename.concat

let with_dir path fn =
  let ch = Unix.opendir path in
  Fun.protect ~finally:(fun () -> Unix.closedir ch) (fun () -> fn ch)

let list_dir path =
  let rec aux acc ch =
    match Unix.readdir ch with
    | name when name.[0] <> '.' -> aux (name :: acc) ch
    | _ -> aux acc ch
    | exception End_of_file -> acc
  in
  with_dir path (aux [])

type t = {
  env : string -> OpamVariable.variable_contents option;
  packages_dirs : string list;
  pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
  constraints : OpamFormula.version_constraint OpamTypes.name_map; (* User-provided constraints *)
  test : OpamPackage.Name.Set.t;
  prefer_oldest : bool;
}

let load t pkg =
  let { OpamPackage.name; version = _ } = pkg in
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (_, opam) -> opam
  | None ->
      List.find_map
        (fun packages_dir ->
          let opam = packages_dir / OpamPackage.Name.to_string name / OpamPackage.to_string pkg / "opam" in
          if Sys.file_exists opam then Some opam else None)
        t.packages_dirs
      |> Option.get |> OpamFilename.raw |> OpamFile.make |> OpamFile.OPAM.read

let user_restrictions t name = OpamPackage.Name.Map.find_opt name t.constraints
let dev = OpamPackage.Version.of_string "dev"

let std_env ?(ocaml_native = true) ?sys_ocaml_version ?opam_version ~arch ~os ~os_distribution ~os_family ~os_version () = function
  | "arch" -> Some (OpamTypes.S arch)
  | "os" -> Some (OpamTypes.S os)
  | "os-distribution" -> Some (OpamTypes.S os_distribution)
  | "os-version" -> Some (OpamTypes.S os_version)
  | "os-family" -> Some (OpamTypes.S os_family)
  | "opam-version" -> Some (OpamVariable.S (Option.value ~default:OpamVersion.(to_string current) opam_version))
  | "sys-ocaml-version" -> sys_ocaml_version |> Option.map (fun v -> OpamTypes.S v)
  | "ocaml:native" -> Some (OpamTypes.B ocaml_native)
  | "enable-ocaml-beta-repository" -> None (* Fake variable? *)
  | v ->
      OpamConsole.warning "Unknown variable %S" v;
      None

let env t pkg v =
  if List.mem v OpamPackageVar.predefined_depends_variables then None
  else
    match OpamVariable.Full.to_string v with
    | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
    | x -> t.env x

let filter_deps t pkg f =
  let dev = OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
  let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
  f |> OpamFilter.partial_filter_formula (env t pkg) |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev ~dev_setup:false ~default:false

let version_compare t (v1, v1_avoid, _) (v2, v2_avoid, _) =
  match (v1_avoid, v2_avoid) with
  | true, true
  | false, false ->
      if t.prefer_oldest then OpamPackage.Version.compare v1 v2 else OpamPackage.Version.compare v2 v1
  | true, false -> 1
  | false, true -> -1

let candidates t name =
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (version, opam) -> [ (version, Ok opam) ]
  | None ->
      let versions =
        List.concat_map
          (fun packages_dir ->
            try packages_dir / OpamPackage.Name.to_string name |> list_dir with
            | Unix.Unix_error (Unix.ENOENT, _, _) -> [])
          t.packages_dirs
        |> List.sort_uniq compare
      in
      let user_constraints = user_restrictions t name in
      versions
      |> List.filter_map (fun dir ->
             match OpamPackage.of_string_opt dir with
             | Some pkg ->
                 List.find_opt (fun packages_dir -> Sys.file_exists (packages_dir / OpamPackage.Name.to_string name / dir / "opam")) t.packages_dirs
                 |> Option.map (fun _ -> OpamPackage.version pkg)
             | _ -> None)
      |> List.filter_map (fun v ->
             let pkg = OpamPackage.create name v in
             let opam = load t pkg in
             let avoid = OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam in
             let available = OpamFile.OPAM.available opam in
             match OpamFilter.eval_to_bool ~default:false (env t pkg) available with
             | true -> Some (v, avoid, opam)
             | false -> None)
      (* https://github.com/ocaml-opam/opam-0install-cudf/issues/5 cf 4.12.1 *)
      |> (fun l -> if List.for_all (fun (_, avoid, _) -> avoid) l then [] else l)
      |> List.sort (version_compare t)
      |> List.map (fun (v, avoid, opam) -> let () = Printf.printf "%s.%s\n" (OpamPackage.Name.to_string name) (OpamPackage.Version.to_string v) in (v, avoid, opam))
      |> List.map (fun (v, _, opam) ->
             match user_constraints with
             | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) -> (v, Error (UserConstraint (name, Some test)))
             | _ -> (v, Ok opam))

let pp_rejection f = function
  | UserConstraint x -> Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)
  | Unavailable -> Fmt.string f "Availability condition not satisfied"

let create ?(prefer_oldest = false) ?(test = OpamPackage.Name.Set.empty) ?(pins = OpamPackage.Name.Map.empty) ~constraints ~env packages_dirs =
  { env; packages_dirs; pins; constraints; test; prefer_oldest }
