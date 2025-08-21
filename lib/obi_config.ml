open Cmdliner

type t = {
  opam_repository : string;
  download_cache : string;
  work_dir : string;
  ocaml_versions : string list;
  hostname : string;
  env : (string * string) list;
  limit : int option; (* None = all packages, Some n = limit to n packages *)
}

let default = {
  opam_repository = "opam-repository";  (* relative to work_dir *)
  download_cache = "download-cache";    (* relative to work_dir *)
  work_dir = "day29";                   (* relative to current directory *)
  ocaml_versions = ["5.3.0"; "4.14.2"; "4.08.2"];
  hostname = "builder";
  env = [
    ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
    ("HOME", "/home/opam");
    ("OPAMYES", "1");
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
  ];
  limit = None;
}

let opam_repository =
  let doc = "Path to the opam repository" in
  Arg.(value & opt string default.opam_repository & info ["opam-repository"] ~docv:"PATH" ~doc)

let download_cache =
  let doc = "Path to the download cache directory" in
  Arg.(value & opt string default.download_cache & info ["download-cache"] ~docv:"PATH" ~doc)

let work_dir =
  let doc = "Working directory for builds" in
  Arg.(value & opt string default.work_dir & info ["work-dir"; "w"] ~docv:"PATH" ~doc)

let ocaml_versions =
  let doc = "OCaml versions to test against" in
  Arg.(value & opt (list string) default.ocaml_versions & info ["ocaml-versions"] ~docv:"VERSIONS" ~doc)

let hostname =
  let doc = "Hostname to use in containers" in
  Arg.(value & opt string default.hostname & info ["hostname"] ~docv:"NAME" ~doc)

let limit =
  let doc = "Limit number of packages to build for testing (default: unlimited)" in
  Arg.(value & opt (some int) None & info ["limit"] ~docv:"N" ~doc)

(* TOML serialization - simplified approach *)
let to_toml_string config =
  let env_section = 
    List.map (fun (k, v) -> Printf.sprintf "%s = \"%s\"" k v) config.env
    |> String.concat "\n"
  in
  let versions_section = 
    List.map (fun v -> Printf.sprintf "\"%s\"" v) config.ocaml_versions
    |> String.concat ", "
  in
  let limit_section = 
    match config.limit with
    | None -> ""
    | Some n -> Printf.sprintf "limit = %d\n" n
  in
  Printf.sprintf {|opam_repository = "%s"
download_cache = "%s"
work_dir = "%s"
ocaml_versions = [%s]
hostname = "%s"
%s
[env]
%s|} config.opam_repository config.download_cache config.work_dir versions_section config.hostname limit_section env_section

let from_toml toml_table =
  try
    let opam_repository = 
      match Toml.Types.Table.find (Toml.Types.Table.Key.of_string "opam_repository") toml_table with
      | Toml.Types.TString s -> s
      | _ -> default.opam_repository
    in
    let download_cache = 
      match Toml.Types.Table.find (Toml.Types.Table.Key.of_string "download_cache") toml_table with
      | Toml.Types.TString s -> s
      | _ -> default.download_cache
    in
    let work_dir = 
      match Toml.Types.Table.find (Toml.Types.Table.Key.of_string "work_dir") toml_table with
      | Toml.Types.TString s -> s
      | _ -> default.work_dir
    in
    let hostname = 
      match Toml.Types.Table.find (Toml.Types.Table.Key.of_string "hostname") toml_table with
      | Toml.Types.TString s -> s
      | _ -> default.hostname
    in
    let ocaml_versions = 
      match Toml.Types.Table.find (Toml.Types.Table.Key.of_string "ocaml_versions") toml_table with
      | Toml.Types.TArray (Toml.Types.NodeString arr) -> arr
      | _ -> default.ocaml_versions
    in
    let limit = 
      match Toml.Types.Table.find (Toml.Types.Table.Key.of_string "limit") toml_table with
      | Toml.Types.TInt n -> Some n
      | _ -> None
    in
    let env = 
      match Toml.Types.Table.find (Toml.Types.Table.Key.of_string "env") toml_table with
      | Toml.Types.TTable env_table ->
          Toml.Types.Table.fold (fun k v acc ->
            match v with
            | Toml.Types.TString s -> (Toml.Types.Table.Key.to_string k, s) :: acc
            | _ -> acc
          ) env_table []
      | _ -> default.env
    in
    Some { opam_repository; download_cache; work_dir; ocaml_versions; hostname; env; limit }
  with
  | _ -> None

let load_config_file path =
  try
    if Sys.file_exists path then
      let toml_result = Toml.Parser.from_filename path in
      match toml_result with
      | `Ok toml_table -> from_toml toml_table
      | `Error _ -> None
    else
      None
  with
  | _ -> None

let save_config_file path config =
  try
    let toml_content = to_toml_string config in
    let oc = open_out path in
    output_string oc toml_content;
    close_out oc;
    true
  with
  | _ -> false

(* Environment variable helpers *)
let get_env_var name default_val =
  try Sys.getenv name with Not_found -> default_val

let from_env_vars () =
  {
    opam_repository = get_env_var "OBI_OPAM_REPOSITORY" default.opam_repository;
    download_cache = get_env_var "OBI_DOWNLOAD_CACHE" default.download_cache;
    work_dir = get_env_var "OBI_WORK_DIR" default.work_dir;
    ocaml_versions = (
      let versions_str = get_env_var "OBI_OCAML_VERSIONS" "" in
      if versions_str = "" then default.ocaml_versions
      else String.split_on_char ',' versions_str |> List.map String.trim
    );
    hostname = get_env_var "OBI_HOSTNAME" default.hostname;
    limit = (
      let limit_str = get_env_var "OBI_LIMIT" "" in
      if limit_str = "" then None
      else try Some (int_of_string limit_str) with _ -> None
    );
    env = default.env; (* Keep default env for now *)
  }

let get_config ?config_file () =
  (* Priority: defaults < config file < env vars < CLI args (handled by cmdliner) *)
  let base_config = default in
  let config_from_file = 
    match config_file with
    | Some path -> load_config_file path |> Option.value ~default:base_config
    | None -> 
        (* Try default config file locations *)
        let home_config = 
          try Some (Filename.concat (Sys.getenv "HOME") ".config/obi.toml")
          with Not_found -> None
        in
        let default_paths = 
          [".obi.toml"] @ (Option.to_list home_config)
        in
        List.fold_left (fun acc path ->
          match acc with
          | Some config -> Some config
          | None -> load_config_file path
        ) None default_paths |> Option.value ~default:base_config
  in
  let config_from_env = from_env_vars () in
  (* Merge: config file as base, override with env vars *)
  {
    opam_repository = if config_from_env.opam_repository <> default.opam_repository then config_from_env.opam_repository else config_from_file.opam_repository;
    download_cache = if config_from_env.download_cache <> default.download_cache then config_from_env.download_cache else config_from_file.download_cache;
    work_dir = if config_from_env.work_dir <> default.work_dir then config_from_env.work_dir else config_from_file.work_dir;
    ocaml_versions = if config_from_env.ocaml_versions <> default.ocaml_versions then config_from_env.ocaml_versions else config_from_file.ocaml_versions;
    hostname = if config_from_env.hostname <> default.hostname then config_from_env.hostname else config_from_file.hostname;
    limit = if config_from_env.limit <> default.limit then config_from_env.limit else config_from_file.limit;
    env = config_from_file.env;
  }

let config_file_arg =
  let doc = "Path to configuration file (TOML format)" in
  Arg.(value & opt (some string) None & info ["config"] ~docv:"FILE" ~doc)

let config_term =
  let combine config_file opam_repository download_cache work_dir ocaml_versions hostname limit =
    (* Load base config from file and env vars *)
    let base_config = get_config ?config_file () in
    (* Override with CLI arguments (only if they differ from defaults) *)
    let final_config = {
      opam_repository = if opam_repository <> default.opam_repository then opam_repository else base_config.opam_repository;
      download_cache = if download_cache <> default.download_cache then download_cache else base_config.download_cache;
      work_dir = if work_dir <> default.work_dir then work_dir else base_config.work_dir;
      ocaml_versions = if ocaml_versions <> default.ocaml_versions then ocaml_versions else base_config.ocaml_versions;
      hostname = if hostname <> default.hostname then hostname else base_config.hostname;
      limit = if limit <> default.limit then limit else base_config.limit;
      env = base_config.env;
    } in
    final_config
  in
  Term.(const combine $ config_file_arg $ opam_repository $ download_cache $ work_dir $ ocaml_versions $ hostname $ limit)

(* Helper functions for path construction *)
let opam_repository_path config = function
  | [] -> config.opam_repository
  | paths -> List.fold_left Filename.concat config.opam_repository paths

let work_dir_path config = function
  | [] -> config.work_dir
  | paths -> List.fold_left Filename.concat config.work_dir paths

let download_cache_path config = config.download_cache