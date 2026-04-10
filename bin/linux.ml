type t = {
  config : Config.t;
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
                   ((if network then [] else [ "network" ]) @ [ "pid"; "ipc"; "uts"; "mount"; "user" ])) );
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
  { config; uid = 1000; gid = 1000 }

let deinit ~t:_ = ()
let config ~t = t.config

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
  match config.os_family with
  | "debian" -> Docker.debian ~config ~temp_dir opam_repository build_log t.uid t.gid
  | os_family ->
      failwith (Printf.sprintf "Unsupported OS family '%s' for Linux container. Currently supported: debian" os_family)

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let os_key = Config.os_key ~config in
  let lowerdir = Path.(temp_dir / "lower") in
  let upperdir = Path.(temp_dir / "fs") in
  let workdir = Path.(temp_dir / "work") in
  let rootfsdir = Path.(temp_dir / "rootfs") in
  let () = List.iter Os.mkdir [ lowerdir; upperdir; workdir; rootfsdir ] in
  let pkg_string = OpamPackage.to_string pkg in
  let pin = if OpamPackage.name_to_string pkg = config.package then [ "opam pin -yn " ^ pkg_string ^ " $HOME/src/"; "cd src" ] else [] in
  let with_test = if config.with_test && OpamPackage.name_to_string pkg = config.package then "--with-test " else "" in
  let argv = [ "/usr/bin/env"; "bash"; "-c"; String.concat " && " (pin @ [ "opam-build -v " ^ with_test ^ pkg_string ]) ] in
  let () =
    List.iter
      (fun hash ->
        Os.hardlink_tree
          ~source:Path.(config.dir / os_key / hash / "fs")
          ~target:lowerdir)
      ordered_hashes
  in
  let () =
    let packages_dir = Path.(lowerdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages") in
    let state_file = Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "switch-state") in
    if Sys.file_exists packages_dir then Opamh.dump_state packages_dir state_file
  in
  let etc_hosts = Path.(temp_dir / "hosts") in
  let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ hostname) in
  let ld = "lowerdir=" ^ String.concat ":" [ lowerdir; Path.(config.dir / os_key / "base" / "fs") ] in
  let ud = "upperdir=" ^ upperdir in
  let wd = "workdir=" ^ workdir in
  let mounts =
    [
      { Mount.ty = "bind"; src = Path.(temp_dir / "opam-repository"); dst = "/home/opam/.opam/repo/default"; options = [ "rbind"; "rprivate" ] };
      { ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
    ]
  in
  let mounts =
    match config.directory with
    | None -> mounts
    | Some src -> mounts @ [ { ty = "bind"; src; dst = "/home/opam/src"; options = [ "rw"; "rbind"; "rprivate" ] } ]
  in
  let config_runc = make ~root:rootfsdir ~cwd:"/home/opam" ~argv ~hostname ~uid:t.uid ~gid:t.gid ~env ~mounts ~network:true in
  let () = Os.write_to_file Path.(temp_dir / "config.json") (Yojson.Safe.pretty_to_string config_runc) in
  (* Run mount + runc + umount inside a user namespace so no sudo is needed.
     Inside the namespace we are mapped to root and can use kernel overlayfs. *)
  let container_name = Filename.basename temp_dir in
  let overlay_opts = String.concat "," [ ld; ud; wd ] in
  let result = Os.unshare_exec ~stdout:build_log ~stderr:build_log [
    "sh"; "-c";
    Printf.sprintf "mount -t overlay overlay %s -o %s && runc run -b %s %s; rc=$?; umount %s; exit $rc"
      (Filename.quote rootfsdir) (Filename.quote overlay_opts)
      (Filename.quote temp_dir) (Filename.quote container_name)
      (Filename.quote rootfsdir)
  ] in
  (* Cleanup — files are user-owned, no sudo needed *)
  let () =
    List.iter
      (fun p -> if Sys.file_exists p then Os.rm ~recursive:true p)
      [
        lowerdir;
        workdir;
        rootfsdir;
        Path.(upperdir / "tmp");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "sources");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "build");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages" / "cache");
      ]
  in
  let () =
    let repo_dir = Path.(upperdir / "home" / "opam" / ".opam" / "repo") in
    if Sys.file_exists repo_dir then
      Array.iter
        (fun f -> if String.length f > 6 && String.sub f 0 6 = "state-" && Filename.check_suffix f ".cache" then
           Sys.remove Path.(repo_dir / f))
        (Sys.readdir repo_dir)
  in
  result
