type t = {
  config : Config.t;
  running_as_root : bool;
  uid : int;
  gid : int;
}

let hostname = "builder"

let env =
  [
    ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
    ("HOME", "/home/opam");
    ("OPAMYES", "1");
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
  ]

let std_env ~(config : Config.t) =
  Util.std_env ~arch:config.arch ~os:"linux" ~os_distribution:"debian" ~os_family:"debian" ~os_version:"13" ~ocaml_version:config.ocaml_version ()

(* This is a subset of the capabilities that Docker uses by default.
     These control what root can do in the container.
     If the init process is non-root, permitted, effective and ambient sets are cleared.
     See capabilities(7) for full details. *)
let default_linux_caps =
  [
    (* Make arbitrary changes to file UIDs and GIDs *)
    "CAP_CHOWN";
    (* Bypass file read, write, and execute permission checks. *)
    "CAP_DAC_OVERRIDE";
    (* Set SUID/SGID bits. *)
    "CAP_FSETID";
    (* Bypass permission checks. *)
    "CAP_FOWNER";
    (* Create special files using mknod. *)
    "CAP_MKNOD";
    (* Make arbitrary manipulations of process GIDs. *)
    "CAP_SETGID";
    (* Make arbitrary manipulations of process UIDs. *)
    "CAP_SETUID";
    (* Set arbitrary capabilities on a file. *)
    "CAP_SETFCAP";
    (* Add any capability from bounding set to inheritable set. *)
    "CAP_SETPCAP";
    (* Use chroot. *)
    "CAP_SYS_CHROOT";
    (* Bypass permission checks for sending signals. *)
    "CAP_KILL";
    (* Write records to kernel auditing log. *)
    "CAP_AUDIT_WRITE";
  ]

let strings xs = `List (List.map (fun x -> `String x) xs)

let make ~root ~cwd ~argv ~hostname ~uid ~gid ~env ~mounts ~network : Yojson.Safe.t =
  `Assoc
    [
      ("ociVersion", `String "1.0.1-dev");
      ( "process",
        `Assoc
          [
            ("terminal", `Bool false);
            ("user", `Assoc [ ("uid", `Int uid); ("gid", `Int gid) ]);
            ("args", strings argv);
            ("env", strings (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) env));
            ("cwd", `String cwd);
            ( "capabilities",
              `Assoc
                [
                  (* Limits capabilities gained on execve. *)
                  ("bounding", strings default_linux_caps);
                  (* Checked by kernel to decide access *)
                  ("effective", strings default_linux_caps);
                  (* Preserved across an execve (if root, or cap in ambient set) *)
                  ("inheritable", strings default_linux_caps);
                  (* Limiting superset for the effective capabilities *)
                  ("permitted", strings default_linux_caps);
                ] );
            ("rlimits", `List [ `Assoc [ ("type", `String "RLIMIT_NOFILE"); ("hard", `Int 1024); ("soft", `Int 1024) ] ]);
            ("noNewPrivileges", `Bool false);
          ] );
      ("root", `Assoc [ ("path", `String root); ("readonly", `Bool false) ]);
      ("hostname", `String hostname);
      ( "mounts",
        `List
          (Mount.user_mounts mounts
          @ [
              Mount.make "/proc" ~options:[ (* TODO: copy to others? *) "nosuid"; "noexec"; "nodev" ] ~ty:"proc" ~src:"proc";
              Mount.make "/tmp" ~ty:"tmpfs" ~src:"tmpfs" ~options:[ "nosuid"; "noatime"; "nodev"; "noexec"; "mode=1777" ];
              Mount.make "/dev" ~ty:"tmpfs" ~src:"tmpfs" ~options:[ "nosuid"; "strictatime"; "mode=755"; "size=65536k" ];
              Mount.make "/dev/pts" ~ty:"devpts" ~src:"devpts" ~options:[ "nosuid"; "noexec"; "newinstance"; "ptmxmode=0666"; "mode=0620"; "gid=5" (* tty *) ];
              Mount.make "/sys" (* This is how Docker does it. runc's default is a bit different. *) ~ty:"sysfs" ~src:"sysfs"
                ~options:[ "nosuid"; "noexec"; "nodev"; "ro" ];
              Mount.make "/sys/fs/cgroup" ~ty:"cgroup" ~src:"cgroup" ~options:[ "ro"; "nosuid"; "noexec"; "nodev" ];
              Mount.make "/dev/shm" ~ty:"tmpfs" ~src:"shm" ~options:[ "nosuid"; "noexec"; "nodev"; "mode=1777"; "size=65536k" ];
              Mount.make "/dev/mqueue" ~ty:"mqueue" ~src:"mqueue" ~options:[ "nosuid"; "noexec"; "nodev" ];
            ]
          @ if network then [ Mount.make "/etc/resolv.conf" ~ty:"bind" ~src:"/etc/resolv.conf" ~options:[ "ro"; "rbind"; "rprivate" ] ] else []) );
      ( "linux",
        `Assoc
          [
            ( "namespaces",
              `List
                (List.map
                   (fun namespace -> `Assoc [ ("type", `String namespace) ])
                   ((if network then [] else [ "network" ]) @ [ "pid"; "ipc"; "uts"; "mount" ])) );
            ( "maskedPaths",
              strings
                [
                  "/proc/acpi";
                  "/proc/asound";
                  "/proc/kcore";
                  "/proc/keys";
                  "/proc/latency_stats";
                  "/proc/timer_list";
                  "/proc/timer_stats";
                  "/proc/sched_debug";
                  "/sys/firmware";
                  "/proc/scsi";
                ] );
            ("readonlyPaths", strings [ "/proc/bus"; "/proc/fs"; "/proc/irq"; "/proc/sys"; "/proc/sysrq-trigger" ]);
            ( "seccomp",
              `Assoc
                ([
                   ("defaultAction", `String "SCMP_ACT_ALLOW");
                   ( "syscalls",
                     `List
                       [
                         `Assoc
                           [
                             (* Sync calls are pointless for the builder, because if the computer crashes then we'll
                               just throw the build dir away and start again. And btrfs sync is really slow.
                               Based on https://bblank.thinkmo.de/using-seccomp-to-filter-sync-operations.html
                               Note: requires runc >= v1.0.0-rc92. *)
                             ("names", strings [ "fsync"; "fdatasync"; "msync"; "sync"; "syncfs"; "sync_file_range" ]);
                             ("action", `String "SCMP_ACT_ERRNO");
                             ("errnoRet", `Int 0);
                             (* Return error "success" *)
                           ];
                       ] );
                 ]
                @ [ ("architectures", strings [ "SCMP_ARCH_X86_64"; "SCMP_ARCH_X86"; "SCMP_ARCH_X32" ]) ]) );
          ] );
    ]

let init ~(config : Config.t) =
  let running_as_root, uid, gid =
    match (Unix.getuid (), Unix.getgid ()) with
    | 0, _ -> (true, 1000, 1000)
    | uid, gid -> (false, uid, gid)
  in
  { config; running_as_root; uid; gid }

let deinit ~t:_ = ()
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
  let _ = Os.sudo [ "/usr/bin/env"; "bash"; "-c"; "docker export $(docker run -d debian:13) | sudo tar -C " ^ rootfs ^ " -x" ] in
  let opam = Path.(rootfs / "/usr/local/bin/opam") in
  let _ = Os.sudo [ "curl"; "-L"; "https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-" ^ config.arch ^ "-linux"; "-o"; opam ] in
  let _ = Os.sudo [ "sudo"; "chmod"; "+x"; opam ] in
  let opam_build = Path.(rootfs / "/usr/local/bin/opam-build") in
  let _ = Os.sudo [ "curl"; "-L"; "https://github.com/mtelvers/opam-build/releases/download/1.1.0/opam-build-1.1.0-" ^ config.arch ^ "-linux"; "-o"; opam_build ] in
  let _ = Os.sudo [ "sudo"; "chmod"; "+x"; opam_build ] in
  let etc_hosts = Path.(temp_dir / "hosts") in
  let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ hostname) in
  let argv =
    [
      "/usr/bin/env";
      "bash";
      "-c";
      String.concat " && "
        [
          "apt update";
          "apt upgrade -y";
          "apt install build-essential unzip bubblewrap git sudo curl rsync -y";
          "groupadd --gid " ^ string_of_int t.gid ^ " opam";
          "adduser --disabled-password --gecos '@opam' --no-create-home --uid " ^ string_of_int t.uid ^ " --gid " ^ string_of_int t.gid
          ^ " --home /home/opam opam";
          "chown -R " ^ string_of_int t.uid ^ ":" ^ string_of_int t.gid ^ " /home/opam";
          {|echo "opam ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/opam|};
          "su - opam -c 'opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y'";
          "su - opam -c 'opam switch create default --empty'";
        ];
    ]
  in
  let mounts =
    [
      { Mount.ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
      { ty = "bind"; src = opam_repository; dst = "/home/opam/opam-repository"; options = [ "rbind"; "rprivate" ] };
    ]
  in
  let config = make ~root:rootfs ~cwd:"/home/opam" ~argv ~hostname ~uid:0 ~gid:0 ~env ~mounts ~network:true in
  let () = Os.write_to_file Path.(temp_dir / "config.json") (Yojson.Safe.pretty_to_string config) in
  let result = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; Filename.basename temp_dir ] in
  let _ = Os.rm Path.(rootfs / "home" / "opam" / ".opam" / "repo" / "state-33BF9E46.cache") in
  result

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let os_key = os_key ~config in
  let lowerdir = Path.(temp_dir / "lower") in
  let upperdir = Path.(temp_dir / "fs") in
  let workdir = Path.(temp_dir / "work") in
  let dummydir = Path.(temp_dir / "dummy") in
  let () = List.iter Os.mkdir [ lowerdir; upperdir; workdir; dummydir ] in
  let pkg_string = OpamPackage.to_string pkg in
  let pin = if pkg_string = config.package then [ "opam pin -yn " ^ pkg_string ^ " $HOME/src/"; "cd src" ] else [] in
  let with_test = if config.with_test then "--with-test " else "" in
  let argv = [ "/usr/bin/env"; "bash"; "-c"; String.concat " && " (pin @ [ "opam-build -v " ^ with_test ^ pkg_string ] ) ] in
  let () =
    List.iter
      (fun hash ->
        assert (
          0
          = Os.sudo
              [
                "cp";
                "--update=none";
                "--archive";
                "--no-dereference";
                "--recursive";
                "--link";
                "--no-target-directory";
                Path.(config.dir / os_key / hash / "fs");
                lowerdir;
              ]))
      ordered_hashes
  in
  let () =
    let packages_dir = Path.(lowerdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages") in
    let state_file = Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "switch-state") in
    if Sys.file_exists packages_dir then Opamh.dump_state packages_dir state_file
  in
  let () =
    if t.running_as_root then
      let home_dir = Path.(upperdir / "home" / "opam") in
      if Sys.file_exists home_dir then ignore(Os.exec [ "chown"; "-R"; string_of_int t.uid ^ ":" ^ string_of_int t.gid; home_dir ])
  in
  let etc_hosts = Path.(temp_dir / "hosts") in
  let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ hostname) in
  let ld = String.concat ":" [ lowerdir; Path.(config.dir / os_key / "base" / "fs") ] in
  let mounts =
    [
      { Mount.ty = "overlay"; src = "overlay"; dst = "/"; options = [ "lowerdir=" ^ ld; "upperdir=" ^ upperdir; "workdir=" ^ workdir ] };
      { ty = "bind"; src = Path.(temp_dir / "opam-repository"); dst = "/home/opam/.opam/repo/default"; options = [ "rbind"; "rprivate" ] };
      { ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
    ]
  in
  let mounts =
    match config.directory with
    | None -> mounts
    | Some src -> mounts @ [ { ty = "bind"; src; dst = "/home/opam/src"; options = [ "rw"; "rbind"; "rprivate" ] } ]
  in
  let config_runc = make ~root:"dummy" ~cwd:"/home/opam" ~argv ~hostname ~uid:t.uid ~gid:t.gid ~env ~mounts ~network:true in
  let () = Os.write_to_file Path.(temp_dir / "config.json") (Yojson.Safe.pretty_to_string config_runc) in
  let result = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; Filename.basename temp_dir ] in
  let _ =
    Os.sudo
      [
        "rm";
        "-rf";
        lowerdir;
        workdir;
        dummydir;
        Path.(upperdir / "tmp");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "sources");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "build");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages" / "cache");
        Path.(upperdir / "home" / "opam" / ".opam" / "repo" / "state-33BF9E46.cache");
      ]
  in
  result
