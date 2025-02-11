type mount = {
  ty : string;
  src : string;
  dst : string;
  options : string list;
}

let mount ~ty ~options ~src dst =
  `Assoc [ ("destination", `String dst); ("type", `String ty); ("source", `String src); ("options", `List (List.map (fun x -> `String x) options)) ]

let user_mounts = List.map @@ fun { ty; src; dst; options } -> mount ~ty ~options ~src dst
let strings xs = `List (List.map (fun x -> `String x) xs)

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

let make ~cwd ~argv ~hostname ~uid ~gid ~env ~mounts ~network : Yojson.Safe.t =
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
      ("root", `Assoc [ ("path", `String "dummy"); ("readonly", `Bool false) ]);
      ("hostname", `String hostname);
      ( "mounts",
        `List
          (user_mounts mounts
          @ [
              mount "/proc" ~options:[ (* TODO: copy to others? *) "nosuid"; "noexec"; "nodev" ] ~ty:"proc" ~src:"proc";
              mount "/dev" ~ty:"tmpfs" ~src:"tmpfs" ~options:[ "nosuid"; "strictatime"; "mode=755"; "size=65536k" ];
              mount "/dev/pts" ~ty:"devpts" ~src:"devpts" ~options:[ "nosuid"; "noexec"; "newinstance"; "ptmxmode=0666"; "mode=0620"; "gid=5" (* tty *) ];
              mount "/sys" (* This is how Docker does it. runc's default is a bit different. *) ~ty:"sysfs" ~src:"sysfs"
                ~options:[ "nosuid"; "noexec"; "nodev"; "ro" ];
              mount "/sys/fs/cgroup" ~ty:"cgroup" ~src:"cgroup" ~options:[ "ro"; "nosuid"; "noexec"; "nodev" ];
              mount "/dev/shm" ~ty:"tmpfs" ~src:"shm" ~options:[ "nosuid"; "noexec"; "nodev"; "mode=1777"; "size=65536k" ];
              mount "/dev/mqueue" ~ty:"mqueue" ~src:"mqueue" ~options:[ "nosuid"; "noexec"; "nodev" ];
            ]
          @ if network then [ mount "/etc/resolv.conf" ~ty:"bind" ~src:"/etc/resolv.conf" ~options:[ "ro"; "rbind"; "rprivate" ] ] else []) );
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
