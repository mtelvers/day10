let std_env ?(ocaml_native = true) ?opam_version ~arch ~os ~os_distribution ~os_family ~os_version ~ocaml_version () = function
  | "arch" -> Some (OpamTypes.S arch)
  | "os" -> Some (OpamTypes.S os)
  | "os-distribution" -> Some (OpamTypes.S os_distribution)
  | "os-version" -> Some (OpamTypes.S os_version)
  | "os-family" -> Some (OpamTypes.S os_family)
  | "opam-version" -> Some (OpamVariable.S (Option.value ~default:OpamVersion.(to_string current) opam_version))
  (* There is no system compliler *)
  | "sys-ocaml-arch"
  | "sys-ocaml-cc"
  | "sys-ocaml-libc"
  | "sys-ocaml-system"
  | "sys-ocaml-version" ->
      Some (OpamTypes.S "")
  | "ocaml:native" -> Some (OpamTypes.B ocaml_native)
  | "ocaml:version" -> Some (OpamTypes.S (OpamPackage.version_to_string ocaml_version))
  | "enable-ocaml-beta-repository" -> None (* Fake variable? *)
  | v ->
      OpamConsole.warning "Unknown variable %S" v;
      None

let save_layer_info name pkg deps hashes rc =
  Yojson.Safe.to_file name
    (`Assoc
       [
         ("package", `String (OpamPackage.to_string pkg));
         ("exit_status", `Int rc);
         ("deps", `List (List.map (fun p -> `String (OpamPackage.to_string p)) deps));
         ("hashes", `List (List.map (fun h -> `String h) hashes));
         ("created", `Float (Unix.time ()));
       ])

let load_layer_info_exit_status name =
  let json = Yojson.Safe.from_file name in
  Yojson.Safe.Util.(json |> member "exit_status" |> to_int)

let load_layer_info_package_name name =
  let json = Yojson.Safe.from_file name in
  Yojson.Safe.Util.(json |> member "package" |> to_string)

let solution_save name pkgs =
  Yojson.Safe.to_file name
    (`Assoc
       (OpamPackage.Map.fold
          (fun pkg deps lst -> (OpamPackage.to_string pkg, `List (OpamPackage.Set.to_list_map (fun p -> `String (OpamPackage.to_string p)) deps)) :: lst)
          pkgs []))

let solution_load name =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file name |> to_assoc
  |> List.fold_left
       (fun acc (s, l) ->
         let pkg = s |> OpamPackage.of_string in
         let deps = l |> to_list |> List.map (fun s -> s |> to_string |> OpamPackage.of_string) |> OpamPackage.Set.of_list in
         OpamPackage.Map.add pkg deps acc)
       OpamPackage.Map.empty

let create_opam_repository path =
  let path = Path.(path / "opam-repository") in
  let () = Os.mkdir path in
  let () = Os.write_to_file Path.(path / "repo") {|opam-version: "2.0"|} in
  path

let git_sha dir =
  let git_dir = Path.(dir / ".git") in
  if not (Sys.file_exists git_dir) then None
  else
    let head = Os.read_from_file Path.(git_dir / "HEAD") |> String.trim in
    if String.length head >= 5 && String.sub head 0 5 = "ref: " then
      let ref_path = String.sub head 5 (String.length head - 5) in
      let loose = Path.(git_dir / ref_path) in
      if Sys.file_exists loose then Some (Os.read_from_file loose |> String.trim)
      else
        (* ref may be packed *)
        let packed_refs = Path.(git_dir / "packed-refs") in
        if not (Sys.file_exists packed_refs) then None
        else
          let packed = Os.read_from_file packed_refs in
          let lines = String.split_on_char '\n' packed in
          let rec find = function
            | [] -> None
            | line :: rest -> (
                match String.split_on_char ' ' line with
                | [ sha; r ] when String.equal r ref_path -> Some sha
                | _ -> find rest)
          in
          find lines
    else Some head

let opam_repo_sha opam_repositories =
  List.filter_map git_sha opam_repositories |> String.concat ""
  |> function "" -> None | s -> Some s

let opam_file opam_repositories pkg =
  List.find_map
    (fun opam_repository ->
      let opam = Path.(opam_repository / "packages" / OpamPackage.name_to_string pkg / OpamPackage.to_string pkg / "opam") in
      if Sys.file_exists opam then Some (OpamFilename.raw opam |> OpamFile.make |> OpamFile.OPAM.read) else None)
    opam_repositories
