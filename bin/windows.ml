type t = {
  config : Config.t;
  network : string;
}

let hostname = "builder"
let env = [ ("OPAMYES", "1"); ("OPAMCONFIRMLEVEL", "unsafe-yes"); ("OPAMERRLOGLEN", "0"); ("OPAMPRECISETRACKING", "1") ]

let std_env ~(config : Config.t) =
  Util.std_env ~arch:"x86_64" ~os:"win32" ~os_distribution:"cygwin" ~os_family:"windows" ~os_version:"10.0.20348" ~ocaml_version:config.ocaml_version ()

let strings xs = `List (List.map (fun x -> `String x) xs)

let make_config_json ~layers ~cwd ~argv ~hostname ~uid ~gid ~env ~mounts ~network : Yojson.Safe.t =
  `Assoc
    [
      ("ociVersion", `String "1.1.0");
      ( "process",
        `Assoc
          [
            ("terminal", `Bool false);
            ("user", `Assoc [ ("uid", `Int uid); ("gid", `Int gid) ]);
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

let init ~(config : Config.t) = { config; network = Os.run "hcn-namespace create" |> String.trim }
let deinit ~t = ignore (Os.exec [ "hcn-namespace"; "delete"; t.network ])
let config ~t = t.config

let layer_hash ~t deps =
  let os =
    List.map
      (fun v -> std_env ~config:t.config v |> Option.map OpamVariable.string_of_variable_contents |> Option.value ~default:"unknown")
      [ "os-family"; "os-version"; "arch" ]
  in
  os @ List.map OpamPackage.to_string deps |> String.concat " " |> Digest.string |> Digest.to_hex

let run ~t ~temp_dir opam_repository build_log =
  let rootfs = Os.path [ temp_dir; "fs" ] in
  let () = Os.mkdir rootfs in
  let argv =
    [
      "cmd";
      "/c";
      String.concat " && "
        [
          "set";
          "curl.exe -L -o c:\\Windows\\opam.exe https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-windows.exe";
          "curl.exe -L -o c:\\Users\\ContainerAdministrator\\AppData\\Local\\opam\\opam-build.exe \
           https://github.com/mtelvers/opam-build/releases/download/1.0.0/opam-build-1.0.0-x86_64-windows.exe";
          (* "net user opam /nopassword /add"; *)
          "opam.exe init -k local -a c:\\opam-repository --bare -y";
          "opam.exe switch create default --empty";
        ];
    ]
  in
  let mounts =
    [
      { Mount.ty = "bind"; src = rootfs; dst = "c:\\Users\\ContainerAdministrator\\AppData\\Local\\opam"; options = [ "rw"; "rbind"; "rprivate" ] };
      (*{ Mount.ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] }; *)
      { ty = "bind"; src = opam_repository; dst = "c:\\opam-repository"; options = [ "rbind"; "rprivate" ] };
    ]
  in
  let mounts_json = Os.path [ temp_dir; "mounts.json" ] in
  let _ =
    Os.retry_exec ~stdout:mounts_json
      [ "ctr"; "snapshot"; "prepare"; "--mounts"; Filename.basename temp_dir; "sha256:5ae66e790cc84572a3bb9646fcbd13b3dbf1af9252e013167791737880626b0b" ]
  in
  let layers = Json_layers.read_layers mounts_json in
  let config = make_config_json ~layers ~cwd:"c:\\" ~argv ~hostname ~uid:0 ~gid:0 ~env ~mounts ~network:t.network in
  let config_json = Os.path [ temp_dir; "config.json" ] in
  let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string config) in
  let result = Os.exec ~stdout:build_log ~stderr:build_log [ "ctr"; "run"; "--cni"; "--rm"; "--config"; config_json; Filename.basename temp_dir ] in
  let _ = Os.rm (Os.path [ rootfs; "lock" ]) in
  let _ = Os.rm (Os.path [ rootfs; "conf.lock" ]) in
  let _ = Os.rm (Os.path [ rootfs; "default"; ".opam-switch"; "lock" ]) in
  let _ = Os.rm (Os.path [ rootfs; "repo"; "state-33BF9E46.cache" ]) in
  let _ = Os.rm (Os.path [ rootfs; "repo"; "conf.lock" ]) in
  let () = Os.write_to_file (Os.path [ temp_dir; "status" ]) (string_of_int result) in
  let _ = Os.exec [ "ctr"; "snapshot"; "rm"; Filename.basename temp_dir ] in
  result

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let target = Os.path [ temp_dir; "fs" ] in
  let () = Os.mkdir target in
  let pin = if OpamPackage.name_to_string pkg = config.package then [ "opam pin -yn " ^ OpamPackage.to_string pkg ^ " $HOME/src/"; "cd src" ] else [] in
  let argv =
    [
      "cmd";
      "/c";
      String.concat " && " (pin @ [ "set && c:\\Users\\ContainerAdministrator\\AppData\\Local\\opam\\opam-build.exe -v " ^ OpamPackage.to_string pkg ]);
    ]
  in
  let _ = Os.hardlink_tree ~source:(Os.path [ config.dir; layer_hash ~t []; "fs" ]) ~target in
  let () = List.iter (fun hash -> Os.hardlink_tree ~source:(Os.path [ config.dir; hash; "fs" ]) ~target) ordered_hashes in
  let () =
    let default_switch = Os.path [ temp_dir; "fs"; "default" ] in
    if Sys.file_exists default_switch then Opamh.dump_state default_switch
  in
  let mounts =
    [
      { Mount.ty = "bind"; src = target; dst = "c:\\Users\\ContainerAdministrator\\AppData\\Local\\opam"; options = [ "rw"; "rbind"; "rprivate" ] };
      {
        ty = "bind";
        src = config.opam_repository;
        dst = "c:\\users\\ContainerAdministrator\\AppData\\Local\\opam\\repo\\default";
        options = [ "rbind"; "rprivate" ];
      };
    ]
  in
  let mounts_json = Os.path [ temp_dir; "mounts.json" ] in
  let _ =
    Os.retry_exec ~stdout:mounts_json
      [ "ctr"; "snapshot"; "prepare"; "--mounts"; Filename.basename temp_dir; "sha256:5ae66e790cc84572a3bb9646fcbd13b3dbf1af9252e013167791737880626b0b" ]
  in
  let layers = Json_layers.read_layers mounts_json in
  let ctr_config = make_config_json ~layers ~cwd:"c:\\" ~argv ~hostname ~uid:0 ~gid:0 ~env ~mounts ~network:t.network in
  let config_json = Os.path [ temp_dir; "config.json" ] in
  let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string ctr_config) in
  let result = Os.exec ~stdout:build_log ~stderr:build_log [ "ctr"; "run"; "--cni"; "--rm"; "--config"; config_json; Filename.basename temp_dir ] in
  let _ = Os.exec [ "ctr"; "snapshot"; "rm"; Filename.basename temp_dir ] in
  let _ = Os.clense_tree ~source:(Os.path [ config.dir; layer_hash ~t []; "fs" ]) ~target in
  let () = List.iter (fun hash -> Os.clense_tree ~source:(Os.path [ config.dir; hash; "fs" ]) ~target) ordered_hashes in
  let _ = Os.rm (Os.path [ target; "repo"; "state-33BF9E46.cache" ]) in
  let _ = Os.rm ~recursive:true (Os.path [ target; "default"; ".opam-switch"; "sources" ]) in
  let _ = Os.rm ~recursive:true (Os.path [ target; "default"; ".opam-switch"; "build" ]) in
  let _ = Os.rm (Os.path [ target; "default"; ".opam-switch"; "lock" ]) in
  result
