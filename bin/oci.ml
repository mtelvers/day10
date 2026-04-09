let oci_arch = function
  | "x86_64" -> "amd64"
  | "aarch64" -> "arm64"
  | "i386"
  | "i686" ->
      "386"
  | "armhf"
  | "armv7l" ->
      "arm"
  | x -> x

let sha256_file path = OpamHash.compute ~kind:`SHA256 path |> OpamHash.contents
let sha256_string s = OpamHash.compute_from_string ~kind:`SHA256 s |> OpamHash.contents

type layer_info = {
  diff_id : string;
  digest : string;
  size : int;
}
[@@deriving yojson]

let ensure_blob ~oci_dir ~oci_cache info =
  let blob_path = Path.(oci_dir / "blobs" / "sha256" / info.digest) in
  if not (Sys.file_exists blob_path) then
    let cached_tar = Path.(oci_cache / "layer.tar.gz") in
    try Unix.link cached_tar blob_path with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | Unix.Unix_error (Unix.EXDEV, _, _) -> Os.cp cached_tar blob_path

let create_layer ~oci_dir ~layer_dir =
  let fs_dir = Path.(layer_dir / "fs") in
  let oci_cache = Path.(layer_dir / "oci") in
  if not (Sys.file_exists oci_cache) then
    Os.create_directory_exclusively oci_cache (fun target_dir ->
        let temp_dir = Filename.temp_dir ~temp_dir:(Filename.dirname layer_dir) ~perms:0o755 "oci-" "" in
        let temp_fs = Path.(temp_dir / "fs") in
        let temp_tar = Path.(temp_dir / "layer.tar") in
        assert (0 = Os.sudo [ "cp"; "--archive"; "--no-dereference"; "--recursive"; "--link"; "--no-target-directory"; fs_dir; temp_fs ]);

        let _ =
          Os.sudo
            [
              "sh";
              "-c";
              Printf.sprintf
                {|find %s -type c 2>/dev/null | while IFS= read -r f; do d=$(dirname "$f"); n=$(basename "$f"); rm -f "$f"; touch "$d/.wh.$n"; done|}
                (Filename.quote temp_fs);
            ]
        in
        let _ = Os.sudo ~stdout:temp_tar [ "tar"; "-C"; temp_fs; "--numeric-owner"; "-c"; "." ] in
        let diff_id = sha256_file temp_tar in
        let _ = Sys.command (Filename.quote_command "gzip" [ "-n"; temp_tar ]) in
        let temp_gz = temp_tar ^ ".gz" in
        let digest = sha256_file temp_gz in
        let size = (Unix.stat temp_gz).st_size in
        Yojson.Safe.to_file Path.(temp_dir / "oci-layer.json") (layer_info_to_yojson { diff_id; digest; size });
        let _ = Os.sudo [ "rm"; "-rf"; temp_fs ] in
        Unix.rename temp_dir target_dir);
  let info =
    match Yojson.Safe.from_file Path.(oci_cache / "oci-layer.json") |> layer_info_of_yojson with
    | Ok info -> info
    | Error msg -> failwith ("Failed to load layer cache: " ^ msg)
  in
  ensure_blob ~oci_dir ~oci_cache info;
  info

let make_config ~arch layer_infos =
  `Assoc
    [
      ("architecture", `String (oci_arch arch));
      ("os", `String "linux");
      ("rootfs", `Assoc [ ("type", `String "layers"); ("diff_ids", `List (List.map (fun li -> `String ("sha256:" ^ li.diff_id)) layer_infos)) ]);
      ( "config",
        `Assoc
          [
            ( "Env",
              `List
                [
                  `String "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
                  `String "HOME=/home/opam";
                  `String "OPAMYES=1";
                  `String "OPAMCONFIRMLEVEL=unsafe-yes";
                  `String "OPAMERRLOGLEN=0";
                  `String "OPAMPRECISETRACKING=1";
                ] );
            ("WorkingDir", `String "/home/opam");
            ("User", `String "1000:1000");
          ] );
    ]

let make_manifest ~config_digest ~config_size layer_infos =
  `Assoc
    [
      ("schemaVersion", `Int 2);
      ("mediaType", `String "application/vnd.oci.image.manifest.v1+json");
      ( "config",
        `Assoc
          [ ("mediaType", `String "application/vnd.oci.image.config.v1+json"); ("digest", `String ("sha256:" ^ config_digest)); ("size", `Int config_size) ] );
      ( "layers",
        `List
          (List.map
             (fun li ->
               `Assoc
                 [ ("mediaType", `String "application/vnd.oci.image.layer.v1.tar+gzip"); ("digest", `String ("sha256:" ^ li.digest)); ("size", `Int li.size) ])
             layer_infos) );
    ]

let update_index ~oci_dir ~manifest_digest ~manifest_size ~arch ~tag =
  let index_path = Path.(oci_dir / "index.json") in
  let lock_path = index_path ^ ".lock" in
  let lock_fd = Unix.openfile lock_path [ O_CREAT; O_WRONLY ] 0o644 in
  Unix.lockf lock_fd F_LOCK 0;
  let new_entry =
    `Assoc
      [
        ("mediaType", `String "application/vnd.oci.image.manifest.v1+json");
        ("digest", `String ("sha256:" ^ manifest_digest));
        ("size", `Int manifest_size);
        ("platform", `Assoc [ ("architecture", `String (oci_arch arch)); ("os", `String "linux") ]);
        ("annotations", `Assoc [ ("org.opencontainers.image.ref.name", `String tag) ]);
      ]
  in
  let existing_manifests =
    if Sys.file_exists index_path then
      let j = Yojson.Safe.from_file index_path in
      Yojson.Safe.Util.(j |> member "manifests" |> to_list)
      |> List.filter (fun m ->
             try
               let open Yojson.Safe.Util in
               let ref_name = m |> member "annotations" |> member "org.opencontainers.image.ref.name" |> to_string in
               ref_name <> tag
             with
             | _ -> true)
    else []
  in
  let index =
    `Assoc
      [ ("schemaVersion", `Int 2); ("mediaType", `String "application/vnd.oci.image.index.v1+json"); ("manifests", `List (existing_manifests @ [ new_entry ])) ]
  in
  Yojson.Safe.to_file index_path index;
  Unix.close lock_fd;
  try Unix.unlink lock_path with
  | _ -> ()

let generate ~(config : Config.t) ~oci_dir ~tag ~layers =
  let os_key = Config.os_key ~config in
  let blobs_dir = Path.(oci_dir / "blobs" / "sha256") in
  Os.mkdir ~parents:true blobs_dir;

  OpamConsole.note "Generating OCI image: %s" tag;

  (* Base layer *)
  let base_dir = Path.(config.dir / os_key / "base") in
  let base_info = create_layer ~oci_dir ~layer_dir:base_dir in

  (* Build layers in topological order (earliest dependency first, target last) *)
  let ordered_layers = List.rev layers in
  let layer_infos =
    List.map
      (fun hash ->
        let layer_dir = Path.(config.dir / os_key / hash) in
        create_layer ~oci_dir ~layer_dir)
      ordered_layers
  in

  let all_layers = base_info :: layer_infos in

  (* Config blob *)
  let config_json = make_config ~arch:config.arch all_layers in
  let config_str = Yojson.Safe.to_string config_json in
  let config_digest = sha256_string config_str in
  let config_size = String.length config_str in
  let config_blob = Path.(blobs_dir / config_digest) in
  if not (Sys.file_exists config_blob) then Os.write_to_file config_blob config_str;

  (* Manifest blob *)
  let manifest_json = make_manifest ~config_digest ~config_size all_layers in
  let manifest_str = Yojson.Safe.to_string manifest_json in
  let manifest_digest = sha256_string manifest_str in
  let manifest_size = String.length manifest_str in
  let manifest_blob = Path.(blobs_dir / manifest_digest) in
  if not (Sys.file_exists manifest_blob) then Os.write_to_file manifest_blob manifest_str;

  (* Update index — uses file locking for concurrent multi-package builds *)
  update_index ~oci_dir ~manifest_digest ~manifest_size ~arch:config.arch ~tag;

  (* OCI layout marker *)
  if not (Sys.file_exists Path.(oci_dir / "oci-layout")) then Os.write_to_file Path.(oci_dir / "oci-layout") {|{"imageLayoutVersion":"1.0.0"}|};

  OpamConsole.note "OCI image '%s': sha256:%s (%d layers)" tag manifest_digest (List.length all_layers)
