type t = {
  config : Config.t;
  uid : int;
  gid : int;
}

let env ~(config : Config.t) =
  [
    ("HOME", "/home/opam");
    ("CI", "true");
    ("OPAMYES", "1");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
    ("OPAMJOBS", string_of_int (Option.value ~default:Os.default_jobs config.opam_jobs));
  ]

(* The release --os-version names, as opam names it: by osreldate, where
   1501000 is 15.1.  None for anything else, which init refuses, since a base
   of some other release would be filed under this one. *)
let release os_version =
  match int_of_string_opt os_version with
  | Some v when v >= 1000000 -> Some (Printf.sprintf "%d.%d-RELEASE" (v / 100000) (v / 1000 mod 100))
  | _ -> None

let init ~(config : Config.t) =
  (* Before anything is made in the cache under the version's name. *)
  let () =
    if Option.is_none (release config.os_version) then
      OpamConsole.error_and_exit `Bad_arguments "--os-version %s: FreeBSD's is an osreldate, such as 1501000 for 15.1" config.os_version
  in
  let uid, gid =
    match (Unix.getuid (), Unix.getgid ()) with
    | 0, _ -> (1000, 1000)
    | uid, gid -> (uid, gid)
  in
  { config; uid; gid }

let deinit ~t:_ = ()
let config ~t = t.config

let fork = Os.fork

(* As opam names it, since the layer key resolves filters against it: uname
   says amd64 where opam in the jail says x86_64. *)
let arch () = OpamSysPoll.arch OpamVariable.Map.empty |> Option.value ~default:(Os.arch ())

(* A layer is root-owned, since the jail ran as root to install depexts, so
   only root can delete it. *)
let remove = Os.sudo_rm
let temp_dir path () = if Sys.file_exists path then Os.quiet "sudo" [ "rm"; "-rf"; path ]

let jail ~temp_dir ~rootfs ~mounts ~env ~argv ~network ~username =
  let jailname = Filename.basename temp_dir in
  let fstab = Path.(temp_dir / "fstab") in
  let () =
    List.map
      (fun (m : Mount.t) ->
        let full = Path.(temp_dir / m.dst) in
        let () = if not (Sys.file_exists full) then ignore (Os.sudo [ "mkdir"; "-p"; full ]) in
        String.concat " " [ m.src; full; m.ty; (if List.mem "ro" m.options then "ro" else "rw"); "0"; "0" ])
      mounts
    |> String.concat "\n" |> Os.write_to_file fstab
  in
  let quote_conf s =
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter
      (fun c ->
        (match c with
        | '"'
        | '\\' ->
            Buffer.add_char buf '\\'
        | _ -> ());
        Buffer.add_char buf c)
      s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  in
  let sh_cmd =
    let env_prefix =
      if env = [] then ""
      else
        let pairs = List.map (fun (k, v) -> k ^ "=" ^ Filename.quote v) env in
        "export " ^ String.concat " " pairs ^ "; "
    in
    env_prefix ^ String.concat " && " argv
  in
  let command_value =
    match username with
    | None -> sh_cmd
    | Some u -> "/usr/bin/su -l " ^ u ^ " -c " ^ Filename.quote sh_cmd
  in
  let network_lines =
    if network then [ "ip4 = inherit;"; "ip6 = inherit;"; "host = inherit;" ]
    else [ "vnet;"; "exec.start = " ^ quote_conf "/sbin/ifconfig lo0 127.0.0.1/8" ^ ";" ]
  in
  let conf =
    let indent = List.map (fun l -> "  " ^ l) in
    String.concat "\n"
      ([ jailname ^ " {" ]
      @ indent
          ([ "path = " ^ quote_conf rootfs ^ ";"; "mount.devfs;"; "mount.fstab = " ^ quote_conf fstab ^ ";" ]
          @ network_lines
          @ [ "command = " ^ quote_conf command_value ^ ";" ])
      @ [ "}" ])
    ^ "\n"
  in
  let conf_file = Path.(temp_dir / "jail.conf") in
  let () = Os.write_to_file conf_file conf in
  [ "jail"; "-f"; conf_file; "-c"; jailname ]

let run ~t ~temp_dir opam_repository build_log =
  let config = t.config in
  let rootfs = Path.(temp_dir / "fs") in
  let () = Os.mkdir rootfs in
  (* The release is the one the platform is keyed on, and freebsd-update is told
     so rather than left to read the running kernel's, so that a jail of an
     older release than the host is patched as what it is.  The mirror lays
     releases out by uname -m and then uname -p, which differ on arm64:
     releases/arm64/aarch64.  Fetched and unpacked directly: bsdinstall
     script, which used to do this, partitions a disk first and on 15.1 waits
     forever for one to be named. *)
  let release = Option.get (release config.os_version) in
  let distsite =
    let machine = Os.capture "uname" [ "-m" ] |> String.trim and processor = Os.capture "uname" [ "-p" ] |> String.trim in
    Printf.sprintf "https://download.freebsd.org/ftp/releases/%s/%s/%s" machine processor release
  in
  (* Every step adds to the one log, and shows it as it happens under --log. *)
  let fetched = Os.exec ~stdout:build_log ~stderr:build_log ~append:true ~tee:config.log in
  let logged = Os.sudo ~stdout:build_log ~stderr:build_log ~append:true ~tee:config.log in
  let manifest = Path.(temp_dir / "MANIFEST") and archive = Path.(temp_dir / "base.txz") in
  (* The mirror carries the supported releases; one that has gone from it is
     not one to build a base of, since freebsd-update has nothing for it
     either. *)
  let fetch file path =
    match fetched [ "curl"; "--fail"; "-L"; "-o"; path; distsite ^ "/" ^ file ] with
    | 0 -> ()
    | _ -> failwith (Printf.sprintf "%s/%s could not be fetched: is %s a supported release?" distsite file release)
  in
  let () = fetch "MANIFEST" manifest in
  let () = fetch "base.txz" archive in
  (* The manifest's line for the archive: its name, its sha256, and the rest. *)
  let () =
    let expected =
      Os.read_from_file manifest |> String.split_on_char '\n'
      |> List.find_map (fun line -> match String.split_on_char '\t' line with "base.txz" :: sum :: _ -> Some sum | _ -> None)
    in
    let actual = Os.capture "sha256" [ "-q"; archive ] |> String.trim in
    if expected <> Some actual then failwith (Printf.sprintf "%s does not match the checksum in %s" archive manifest)
  in
  let _ = logged [ "tar"; "-xpf"; archive; "-C"; rootfs ] in
  let () = List.iter Os.rm [ archive; manifest ] in
  let _ = Os.sudo [ "cp"; "/etc/resolv.conf"; Path.(rootfs / "etc" / "resolv.conf") ] in
  let _ = logged [ "freebsd-update"; "--not-running-from-cron"; "--currently-running"; release; "-b"; rootfs; "fetch"; "install" ] in
  let _ = logged [ "pkg"; "--chroot"; rootfs; "install"; "-y"; "pkg" ] in
  let _ = logged [ "pkg"; "--chroot"; rootfs; "upgrade"; "-y"; "-f" ] in
  let opam = Path.(rootfs / "usr" / "bin" / "opam") in
  let arch = if config.arch = "amd64" then "x86_64" else config.arch in
  (* --fail, or a missing release is downloaded as a page of HTML and fails
     later, as something else. *)
  let _ = logged [ "curl"; "--fail"; "-L"; "https://github.com/ocaml/opam/releases/download/2.5.1/opam-2.5.1-" ^ arch ^ "-freebsd"; "-o"; opam ] in
  let _ = Os.sudo [ "chmod"; "+x"; opam ] in
  let day10_install = Path.(rootfs / "usr" / "bin" / "day10-install") in
  let _ = logged [ "curl"; "--fail"; "-L"; "https://github.com/mtelvers/day10-install/releases/download/1.4.0/day10-install-1.4.0-" ^ arch ^ "-freebsd"; "-o"; day10_install ] in
  let _ = Os.sudo [ "chmod"; "+x"; day10_install ] in
  let argv =
    [
      "pw groupadd opam -g " ^ string_of_int t.gid;
      "pw useradd -m -n opam -g opam -u " ^ string_of_int t.uid ^ " -h - -c opam";
      "pkg install -y sudo gmake git patch rsync bash zstd pkgconf";
      {|echo "opam ALL=(ALL:ALL) NOPASSWD:ALL" > /usr/local/etc/sudoers.d/opam|};
    ]
  in
  let result = logged (jail ~temp_dir ~rootfs ~mounts:[] ~env:[] ~argv ~network:true ~username:None) in
  let () = if result = 0 then ignore (Os.sudo [ "umount"; Path.(rootfs / "dev") ]) in
  let _ = Os.sudo [ "chflags"; "-R"; "0"; rootfs ] in
  let argv =
    [ "touch /home/opam/.hushlogin"; "opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y"; "opam switch create default --empty" ]
  in
  let mounts = [ { Mount.ty = "nullfs"; src = opam_repository; dst = Path.("fs" / "home" / "opam" / "opam-repository"); options = [ "ro" ] } ] in
  let result = logged (jail ~temp_dir ~rootfs ~mounts ~env:(env ~config) ~argv ~network:true ~username:(Some "opam")) in
  let () =
    if result = 0 then (
      ignore (Os.sudo [ "umount"; Path.(rootfs / "dev") ]);
      ignore (Os.sudo [ "umount"; "-a"; "-f"; "-F"; Path.(temp_dir / "fstab") ]))
  in
  let _ = Os.sudo [ "rm"; "-rf"; Path.(rootfs / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "environment") ] in
  let _ = Os.sudo [ "sh"; "-c"; ("rm -f " ^ Path.(rootfs / "home" / "opam" / ".opam" / "repo" / "state-*.cache")) ] in
  let () = Os.write_to_file Path.(temp_dir / "status") (string_of_int result) in
  result

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let os_key = Config.os_key ~config in
  let lowerdir = Path.(temp_dir / "lower") in
  let upperdir = Path.(temp_dir / "fs") in
  let workdir = Path.(temp_dir / "work") in
  let () = List.iter Os.mkdir [ lowerdir; upperdir; workdir ] in
  let argv =
    match config.build_command with
    | Some { run; _ } -> [ "cd src"; run ]
    | None -> Build_command.for_package ~config pkg
  in
  let () =
    List.iter
      (fun hash ->
        (* no directory target option on FreeBSD cp *)
        let dir = Path.(config.dir / os_key / hash / "fs") in
        let dirs = Sys.readdir dir |> Array.to_list |> List.map (fun d -> Path.(dir / d)) in
        ignore (Os.sudo ([ "cp"; "-n"; "-a"; "-R"; "-l" ] @ dirs @ [ lowerdir ])))
      (ordered_hashes @ [ "base" ])
  in
  let () =
    let packages_dir = Path.(lowerdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages") in
    let state_file = Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "switch-state") in
    if Sys.file_exists packages_dir then Opamh.dump_state packages_dir state_file
  in
  let mounts =
    [
      { Mount.ty = "nullfs"; src = lowerdir; dst = "work"; options = [ "ro" ] };
      { Mount.ty = "unionfs"; src = upperdir; dst = "work"; options = [ "rw" ] };
      { ty = "nullfs"; src = Path.(temp_dir / "opam-repository"); dst = Path.("work" / "home" / "opam" / ".opam" / "repo" / "default"); options = [ "ro" ] };
    ]
  in
  let mounts =
    match config.directory with
    | None -> mounts
    | Some src ->
        let () = ignore (Os.sudo [ "mkdir"; "-p"; Path.(upperdir / "home" / "opam" / "src") ]) in
        mounts @ [ { ty = "nullfs"; src; dst = Path.("work" / "home" / "opam" / "src"); options = [ "rw" ] } ]
  in
  (* Shown as it happens for the command the user asked for, as on Linux. *)
  let tee = Option.is_some config.build_command || config.log in
  let result = Os.sudo ~stdout:build_log ~stderr:build_log ~tee (jail ~temp_dir ~rootfs:workdir ~mounts ~env:(env ~config) ~argv ~network:true ~username:(Some "opam")) in
  let () =
    ignore (Os.sudo [ "umount"; Path.(workdir / "dev") ]);
    ignore (Os.sudo [ "umount"; "-a"; "-f"; "-F"; Path.(temp_dir / "fstab") ])
  in
  let _ =
    Os.sudo
      [
        "rm";
        "-rf";
        lowerdir;
        workdir;
        Path.(upperdir / "tmp");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "sources");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "build");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages" / "cache");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "environment");
      ]
  in
  let _ = Os.sudo [ "sh"; "-c"; ("rm -f " ^ Path.(upperdir / "home" / "opam" / ".opam" / "repo" / "state-*.cache")) ] in
  result

