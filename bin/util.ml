type os = [
  Dockerfile_opam.Distro.distro
  | `FreeBSD
]

let std_env ?(ocaml_native = true) ?opam_version ~arch ~(os:os) ~ocaml_version =
  let os, os_family, os_distribution, os_version =
    let host_os_version = OpamSysPoll.os_version OpamVariable.Map.empty in
    match os with
    | `FreeBSD ->
        "freebsd", "bsd", "freebsd", Option.get host_os_version
    | (#Dockerfile_opam.Distro.distro as os) ->
        let version_from_tag =
          let tag = Dockerfile_opam.Distro.tag_of_distro (os :> Dockerfile_opam.Distro.t) in
          String.rindex_opt tag '-'
          |> Option.map (fun i ->
              let i = i + 1 in String.sub tag i (String.length tag - i))
        in
        match os with
        | `Ubuntu _ ->
            "linux", "debian", "ubuntu", Option.get version_from_tag
        | `Debian (`Unstable | `Testing) ->
            "linux", "debian", "debian", "unknown"
        | `Debian _ ->
            "linux", "debian", "debian", Option.get version_from_tag
        | `CentOS _ ->
            "linux", "rhel", "centos", Option.get version_from_tag
        | `Fedora _ ->
            "linux", "fedora", "fedora", Option.get version_from_tag
        | `OracleLinux _ ->
            (* XXX The .0 is not strictly accurate... *)
            "linux", "fedora", "ol", Option.get version_from_tag ^ ".0"
        | `Alpine _ ->
            (* XXX The .0 is not strictly accurate... *)
            "linux", "alpine", "alpine", Option.get version_from_tag ^ ".0"
        | `Archlinux _ ->
            "linux", "arch", "arch", "rolling"
        | `OpenSUSE `Tumbleweed ->
            (* XXX This needs updating or deriving or something *)
            "linux", "opensuse", "opensuse-tumbleweed", "20251113"
        | `OpenSUSE (`V42_1 | `V42_2 | `V42_3) ->
            "linux", "suse", "opensuse", Option.get version_from_tag
        | `OpenSUSE (`V15_0 | `V15_1 | `V15_2 | `V15_3 | `V15_4 | `V15_5 | `V15_6 | `V16_0) ->
            "linux", "suse", "opensuse-leap", Option.get version_from_tag
        | `Cygwin _ ->
            "cygwin", "windows", "cygwin", Option.get host_os_version
        | `Windows _
        | `WindowsServer _ ->
            "win32", "windows", "cygwin", Option.get host_os_version
        
  in
  fun () -> function
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

let opam_file opam_repositories pkg =
  List.find_map
    (fun opam_repository ->
      let opam = Path.(opam_repository / "packages" / OpamPackage.name_to_string pkg / OpamPackage.to_string pkg / "opam") in
      if Sys.file_exists opam then Some (OpamFilename.raw opam |> OpamFile.make |> OpamFile.OPAM.read) else None)
    opam_repositories
