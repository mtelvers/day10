type rejection =
  | UserConstraint of OpamFormula.atom
  | Unavailable

type t = {
  repo : Repo.t;
  pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
  constraints : OpamFormula.version_constraint OpamTypes.name_map;
  test : OpamPackage.Name.Set.t;
  doc : OpamPackage.Name.Set.t;
  prefer_oldest : bool;
  env_fn : string -> OpamVariable.variable_contents option;
}

let create ?(prefer_oldest = false) ?(test = OpamPackage.Name.Set.empty) ?(doc = OpamPackage.Name.Set.empty)
    ?(pins = OpamPackage.Name.Map.empty) ~constraints ~env ~repo () =
  { repo; pins; constraints; test; doc; prefer_oldest; env_fn = env }

let repo t = t.repo
let opam_file t pkg = Repo.opam t.repo pkg

let load t pkg =
  match OpamPackage.Name.Map.find_opt (OpamPackage.name pkg) t.pins with
  | Some (_, opam) -> opam
  | None ->
      match opam_file t pkg with
      | Some v -> v
      | None -> failwith (Printf.sprintf "opam not found for %s" (OpamPackage.to_string pkg))

let user_restrictions t name = OpamPackage.Name.Map.find_opt name t.constraints
let dev = OpamPackage.Version.of_string "dev"

let env t pkg v =
  if List.mem v OpamPackageVar.predefined_depends_variables then None
  else
    match OpamVariable.Full.to_string v with
    | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
    | x -> t.env_fn x

let filter_deps t pkg f =
  let dev = OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
  let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
  let doc = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.doc in
  f
  |> OpamFilter.partial_filter_formula (env t pkg)
  |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc ~dev ~dev_setup:false ~default:false

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
      let versions = Repo.versions t.repo name in
      let user_constraints = user_restrictions t name in
      versions
      |> List.filter_map (fun v ->
             let pkg = OpamPackage.create name v in
             match opam_file t pkg with
             | None -> None
             | Some opam ->
                 let avoid = OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam in
                 let available = OpamFile.OPAM.available opam in
                 if OpamFilter.eval_to_bool ~default:false (env t pkg) available then Some (v, avoid, opam) else None)
      |> (fun l -> if List.for_all (fun (_, avoid, _) -> avoid) l then [] else l)
      |> List.sort (version_compare t)
      |> List.map (fun (v, _, opam) ->
             match user_constraints with
             | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) -> (v, Error (UserConstraint (name, Some test)))
             | _ -> (v, Ok opam))

let pp_rejection f = function
  | UserConstraint x -> Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)
  | Unavailable -> Fmt.string f "Availability condition not satisfied"
