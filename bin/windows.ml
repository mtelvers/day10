type t = {
  config : Config.t;
  network : string;
  username : string;
}

let hostname = "builder"
let env = [ ("OPAMYES", "1"); ("OPAMCONFIRMLEVEL", "unsafe-yes"); ("OPAMERRLOGLEN", "0"); ("OPAMPRECISETRACKING", "1") ]

let std_env ~(config : Config.t) =
  Util.std_env ~arch:config.arch ~os:"win32" ~os_distribution:"cygwin" ~os_family:"windows" ~os_version:"10.0.20348" ~ocaml_version:config.ocaml_version ()

let strings xs = `List (List.map (fun x -> `String x) xs)

let make_config_json ~layers ~cwd ~argv ~hostname ~username ~env ~mounts ~network : Yojson.Safe.t =
  `Assoc
    [
      ("ociVersion", `String "1.1.0");
      ( "process",
        `Assoc
          [
            ("terminal", `Bool false);
            ("user", `Assoc [ ("username", `String username) ]);
            ("args", strings argv);
            ("env", strings (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) env));
            ("cwd", `String cwd);
          ] );
      ("root", `Assoc [ ("path", `String ""); ("readonly", `Bool false) ]);
      ("hostname", `String hostname);
      ("mounts", `List (Mount.user_mounts mounts));
      ( "windows",
        `Assoc
          [
            ("layerFolders", strings layers);
            ("ignoreFlushesDuringBoot", `Bool true);
            ("network", `Assoc [ ("allowUnqualifiedDNSQuery", `Bool true); ("networkNamespace", `String network) ]);
          ] );
    ]

let init ~(config : Config.t) = { config; network = Os.run "hcn-namespace create" |> String.trim; username = "ContainerAdministrator" }
let deinit ~t = ignore (Os.exec [ "hcn-namespace"; "delete"; t.network ])
let config ~t = t.config

let os_key ~config =
  let os =
    List.map
      (fun v -> std_env ~config v |> Option.map OpamVariable.string_of_variable_contents |> Option.value ~default:"unknown")
      [ "os-family"; "os-version"; "arch" ]
  in
  String.concat "-" os

let layer_hash ~t deps =
  let hashes =
    List.map
      (fun opam ->
        opam |> Util.opam_file t.config.opam_repositories |> Option.get |> OpamFile.OPAM.effective_part |> OpamFile.OPAM.write_to_string
        |> OpamHash.compute_from_string |> OpamHash.to_string)
      deps
  in
  String.concat " " hashes |> Digest.string |> Digest.to_hex

let run ~t ~temp_dir opam_repository build_log =
  let config = t.config in
  let rootfs = Path.(temp_dir / "fs") in
  let () = Os.mkdir rootfs in
  let argv =
    [
      "cmd";
      "/c";
      String.concat " && "
        [
          "curl.exe -L -o c:\\Windows\\opam.exe https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-" ^ config.arch ^ "-windows.exe";
          "curl.exe -L -o c:\\Users\\" ^ t.username
          ^ "\\AppData\\Local\\opam\\opam-build.exe https://github.com/mtelvers/opam-build/releases/download/1.0.0/opam-build-1.0.0-" ^ config.arch ^ "-windows.exe";
          (* "net user opam /nopassword /add"; *)
          "opam.exe init -k local -a c:\\opam-repository --bare -y";
          "opam.exe switch create default --empty";
        ];
    ]
  in
  let mounts =
    [
      { Mount.ty = "bind"; src = rootfs; dst = "c:\\Users\\" ^ t.username ^ "\\AppData\\Local\\opam"; options = [ "rw"; "rbind"; "rprivate" ] };
      (*{ Mount.ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] }; *)
      { ty = "bind"; src = opam_repository; dst = "c:\\opam-repository"; options = [ "rbind"; "rprivate" ] };
    ]
  in
  let mounts_json = Path.(temp_dir / "mounts.json") in
  let _ =
    Os.retry_exec ~stdout:mounts_json
      [ "ctr"; "snapshot"; "prepare"; "--mounts"; Filename.basename temp_dir; "sha256:5ae66e790cc84572a3bb9646fcbd13b3dbf1af9252e013167791737880626b0b" ]
  in
  let layers = Json_layers.read_layers mounts_json in
  let config = make_config_json ~layers ~cwd:"c:\\" ~argv ~hostname ~username:t.username ~env ~mounts ~network:t.network in
  let config_json = Path.(temp_dir / "config.json") in
  let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string config) in
  let result = Os.exec ~stdout:build_log ~stderr:build_log [ "ctr"; "run"; "--cni"; "--rm"; "--config"; config_json; Filename.basename temp_dir ] in
  let () = Os.cp Path.(rootfs / ".cygwin" / "root" / "etc" / "setup" / "installed.db") Path.(temp_dir / "installed.db") in
  let () =
    List.iter Os.rm
      ([
         Path.(rootfs / "lock");
         Path.(rootfs / "conf.lock");
         Path.(rootfs / "default" / ".opam-switch" / "lock");
         Path.(rootfs / ".cygwin" / "root" / "etc" / "setup" / "installed.db");
         Path.(rootfs / "default" / ".opam-switch" / "packages" / "cache");
         Path.(rootfs / "default" / ".opam-switch" / "environment");
         Path.(rootfs / "repo" / "conf.lock");
       ]
      @ Os.ls ~extn:".cache" Path.(rootfs / "repo"))
  in
  let () = Os.write_to_file Path.(temp_dir / "status") (string_of_int result) in
  let _ = Os.exec [ "ctr"; "snapshot"; "rm"; Filename.basename temp_dir ] in
  result

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let os_key = os_key ~config in
  let target = Path.(temp_dir / "fs") in
  let () = Os.mkdir target in
  let pin = if OpamPackage.name_to_string pkg = config.package then [ "opam pin -yn " ^ OpamPackage.to_string pkg ^ " $HOME/src/"; "cd src" ] else [] in
  let with_test = if config.with_test then "--with-test " else "" in
  let argv =
    [
      "cmd";
      "/c";
      String.concat " && "
        ([
           "curl.exe -L -o c:\\Windows\\opam.exe https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-" ^ config.arch ^ "-windows.exe";
           "opam option sys-pkg-manager-cmd";
         ]
        @ pin
        @ [ "c:\\Users\\" ^ t.username ^ "\\AppData\\Local\\opam\\opam-build.exe -v " ^ with_test ^ OpamPackage.to_string pkg ]);
    ]
  in
  let sources = ordered_hashes @ [ "base" ] in
  let () = List.iter (fun hash -> Os.copy_tree ~source:Path.(config.dir / os_key / hash / "fs") ~target) sources in
  let lines =
    List.fold_left
      (fun acc hash -> In_channel.with_open_text Path.(config.dir / os_key / hash / "installed.db") @@ fun ic -> acc @ In_channel.input_lines ic)
      [] sources
  in
  let () = Os.write_to_file Path.(target / ".cygwin" / "root" / "etc" / "setup" / "installed.db") (List.sort_uniq compare lines |> String.concat "\n") in
  let () =
    let packages_dir = Path.(target / "default" / ".opam-switch" / "packages") in
    let state_file = Path.(target / "default" / ".opam-switch" / "switch-state") in
    if Sys.file_exists packages_dir then Opamh.dump_state packages_dir state_file
  in
  let mounts =
    [
      { Mount.ty = "bind"; src = target; dst = "c:\\Users\\" ^ t.username ^ "\\AppData\\Local\\opam"; options = [ "rw"; "rbind"; "rprivate" ] };
      {
        ty = "bind";
        src = Path.(temp_dir / "opam-repository");
        dst = "c:\\users\\" ^ t.username ^ "\\AppData\\Local\\opam\\repo\\default";
        options = [ "rbind"; "rprivate" ];
      };
    ]
  in
  let mounts_json = Path.(temp_dir / "mounts.json") in
  let _ =
    Os.retry_exec ~stdout:mounts_json
      [ "ctr"; "snapshot"; "prepare"; "--mounts"; Filename.basename temp_dir; "sha256:5ae66e790cc84572a3bb9646fcbd13b3dbf1af9252e013167791737880626b0b" ]
  in
  let layers = Json_layers.read_layers mounts_json in
  let ctr_config = make_config_json ~layers ~cwd:"c:\\" ~argv ~hostname ~username:t.username ~env ~mounts ~network:t.network in
  let config_json = Path.(temp_dir / "config.json") in
  let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string ctr_config) in
  let result = Os.exec ~stdout:build_log ~stderr:build_log [ "ctr"; "run"; "--cni"; "--rm"; "--config"; config_json; Filename.basename temp_dir ] in
  let _ = Os.exec [ "ctr"; "snapshot"; "rm"; Filename.basename temp_dir ] in
  let () = Os.cp Path.(target / ".cygwin" / "root" / "etc" / "setup" / "installed.db") Path.(temp_dir / "installed.db") in
  let () =
    List.iter Os.rm
      ([
         Path.(target / "default" / ".opam-switch" / "lock");
         Path.(target / "default" / ".opam-switch" / "environment");
         Path.(target / "default" / ".opam-switch" / "packages" / "cache");
         Path.(target / ".cygwin" / "root" / "etc" / "setup" / "installed.db");
       ]
      @ Os.ls ~extn:".cache" Path.(target / "repo"))
  in
  let () = List.iter (Os.rm ~recursive:true) [ Path.(target / "default" / ".opam-switch" / "sources"); Path.(target / "default" / ".opam-switch" / "build") ] in
  let () = List.iter (fun hash -> Os.clense_tree ~source:Path.(config.dir / os_key / hash / "fs") ~target) sources in
  result
