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
  (* If the effective UID is 0 but the actual UID is <> 0 then we have a SUID binary *)
  (* Set the actual UID to 0, as SUID is not inherited *)
  if Unix.geteuid () = 0 && Unix.getuid () <> 0 then Unix.setuid 0;
  if Unix.getegid () = 0 && Unix.getgid () <> 0 then Unix.setgid 0;
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
  match t.config.os with
  | `FreeBSD
  | `Cygwin _
  | `Windows _
  | `WindowsServer _ ->
      assert false
  | (#Dockerfile_opam.Distro.distro as distro) ->
      match Docker.build_base distro t.config.arch ~temp_dir build_log with
      | Some tag ->
          let rootfs = Path.(temp_dir / "fs") in
          let () = Os.mkdir rootfs in
          let _ = Os.sudo [ "/usr/bin/env"; "bash"; "-c"; "docker export $(docker run -d " ^ tag ^ ") | sudo tar -C " ^ rootfs ^ " -x" ] in
          let etc_hosts = Path.(temp_dir / "hosts") in
          let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ hostname) in
          let argv =
            [
              "/usr/bin/env";
              "bash";
              "-c";
              String.concat " && "
                [
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
          let _ = Os.sudo [ "sh"; "-c"; ("rm -f " ^ Path.(rootfs / "home" / "opam" / ".opam" / "repo" / "state-*.cache")) ] in
          result
      | None ->
          1 (* XXX *)

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
    if Sys.file_exists packages_dir then begin
      Opamh.dump_state packages_dir state_file;
      ignore (Os.sudo ["chown"; "-R"; "1000:1000"; upperdir])
    end
  in
  let etc_hosts = Path.(temp_dir / "hosts") in
  let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ hostname) in
  let ld = "lowerdir=" ^ String.concat ":" [ lowerdir; Path.(config.dir / os_key / "base" / "fs") ] in
  let ud = "upperdir=" ^ upperdir in
  let wd = "workdir=" ^ workdir in
  let mount_cmd = [ "mount"; "-t"; "overlay"; "overlay"; rootfsdir; "-o"; String.concat "," [ ld; ud; wd ] ] in
  Os.write_to_file Path.(temp_dir / "mount_rootfs") (String.concat " " mount_cmd);
  let _ = Os.sudo mount_cmd in
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
  let config_runc = make ~root:rootfsdir ~cwd:"/home/opam" ~argv ~hostname ~uid:1000 ~gid:1000 ~env ~mounts ~network:true in
  let () = Os.write_to_file Path.(temp_dir / "config.json") (Yojson.Safe.pretty_to_string config_runc) in
  let result = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; Filename.basename temp_dir ] in
  let _ = Os.sudo [ "umount"; rootfsdir ] in
  let _ =
    Os.sudo
      [
        "rm";
        "-rf";
        lowerdir;
        workdir;
        rootfsdir;
        Path.(upperdir / "tmp");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "sources");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "build");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages" / "cache");
      ]
  in
  let _ = Os.sudo [ "sh"; "-c"; ("rm -f " ^ Path.(upperdir / "home" / "opam" / ".opam" / "repo" / "state-*.cache")) ] in
  result
