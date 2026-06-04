type t = {
  terminal : bool;
  cwd : string;
  hostname : string;
  env : (string * string) list;
  mounts : Mount.t list;
  network : bool;
  argv : string list;
  uid : int;
  gid : int;
  cpuset : string option;
  numa_mems : string option;
}

let make ?(terminal = false) ?(cwd = "/") ?(hostname = "container")
    ?(env = []) ?(mounts = []) ?(network = false)
    ?cpuset ?numa_mems
    ~argv ~uid ~gid () : t =
  { terminal; cwd; hostname; env; mounts; network; argv; uid; gid;
    cpuset; numa_mems }

let default_linux_caps = [
  "CAP_CHOWN";
  "CAP_DAC_OVERRIDE";
  "CAP_FSETID";
  "CAP_FOWNER";
  "CAP_MKNOD";
  "CAP_SETGID";
  "CAP_SETUID";
  "CAP_SETFCAP";
  "CAP_SETPCAP";
  "CAP_SYS_CHROOT";
  "CAP_KILL";
  "CAP_AUDIT_WRITE";
]

let strings xs = `List (List.map (fun x -> `String x) xs)

let to_yojson ~root (t : t) : Yojson.Safe.t =
  `Assoc [
    ("ociVersion", `String "1.0.1-dev");
    ("process", `Assoc [
       ("terminal", `Bool t.terminal);
       ("user", `Assoc [ ("uid", `Int t.uid); ("gid", `Int t.gid) ]);
       ("args", strings t.argv);
       ("env", strings (List.map (fun (k, v) ->
          Printf.sprintf "%s=%s" k v) t.env));
       ("cwd", `String t.cwd);
       ("capabilities", `Assoc [
          ("bounding", strings default_linux_caps);
          ("effective", strings default_linux_caps);
          ("inheritable", strings default_linux_caps);
          ("permitted", strings default_linux_caps);
        ]);
       ("rlimits", `List [
          `Assoc [ ("type", `String "RLIMIT_NOFILE");
                   ("hard", `Int 1024); ("soft", `Int 1024) ] ]);
       ("noNewPrivileges", `Bool false);
     ]);
    ("root", `Assoc [
       ("path", `String root);
       ("readonly", `Bool false) ]);
    ("hostname", `String t.hostname);
    ("mounts", `List (
       List.map Mount.to_json t.mounts
       @ [
         Mount.(to_json { ty = "proc"; src = "proc"; dst = "/proc";
                          options = [ "nosuid"; "noexec"; "nodev" ] });
         Mount.(to_json { ty = "tmpfs"; src = "tmpfs"; dst = "/tmp";
                          options = [ "nosuid"; "noatime"; "nodev";
                                      "noexec"; "mode=1777" ] });
         Mount.(to_json { ty = "tmpfs"; src = "tmpfs"; dst = "/dev";
                          options = [ "nosuid"; "strictatime"; "mode=755";
                                      "size=65536k" ] });
         Mount.(to_json { ty = "devpts"; src = "devpts"; dst = "/dev/pts";
                          options = [ "nosuid"; "noexec"; "newinstance";
                                      "ptmxmode=0666"; "mode=0620";
                                      "gid=5" ] });
         Mount.(to_json { ty = "sysfs"; src = "sysfs"; dst = "/sys";
                          options = [ "nosuid"; "noexec"; "nodev"; "ro" ] });
         Mount.(to_json { ty = "cgroup"; src = "cgroup";
                          dst = "/sys/fs/cgroup";
                          options = [ "ro"; "nosuid"; "noexec"; "nodev" ] });
         Mount.(to_json { ty = "tmpfs"; src = "shm"; dst = "/dev/shm";
                          options = [ "nosuid"; "noexec"; "nodev";
                                      "mode=1777"; "size=65536k" ] });
         Mount.(to_json { ty = "mqueue"; src = "mqueue";
                          dst = "/dev/mqueue";
                          options = [ "nosuid"; "noexec"; "nodev" ] });
       ]
       @ (if t.network then
            [ Mount.(to_json { ty = "bind"; src = "/etc/resolv.conf";
                               dst = "/etc/resolv.conf";
                               options = [ "ro"; "rbind"; "rprivate" ] }) ]
          else [])));
    ("linux", `Assoc (
       (* cgroup cpuset/mems — emitted only when set, so unconfigured
          containers stay identical to the pre-NUMA spec. *)
       (match t.cpuset, t.numa_mems with
        | None, None -> []
        | _ ->
          let cpu_kv = List.filter_map
            (fun (k, v) -> Option.map (fun s -> (k, `String s)) v)
            [ ("cpus", t.cpuset); ("mems", t.numa_mems) ] in
          [ ("resources", `Assoc [ ("cpu", `Assoc cpu_kv) ]) ])
     @ [
       ("namespaces", `List (
          List.map (fun ns -> `Assoc [ ("type", `String ns) ])
            ((if t.network then [] else [ "network" ])
             @ [ "pid"; "ipc"; "uts"; "mount" ])));
       ("maskedPaths", strings [
          "/proc/acpi"; "/proc/asound"; "/proc/kcore"; "/proc/keys";
          "/proc/latency_stats"; "/proc/timer_list"; "/proc/timer_stats";
          "/proc/sched_debug"; "/sys/firmware"; "/proc/scsi" ]);
       ("readonlyPaths", strings [
          "/proc/bus"; "/proc/fs"; "/proc/irq"; "/proc/sys";
          "/proc/sysrq-trigger" ]);
       ("seccomp", `Assoc [
          ("defaultAction", `String "SCMP_ACT_ALLOW");
          ("syscalls", `List [
             `Assoc [
               ("names", strings [
                  "fsync"; "fdatasync"; "msync"; "sync"; "syncfs";
                  "sync_file_range" ]);
               ("action", `String "SCMP_ACT_ERRNO");
               ("errnoRet", `Int 0);
             ]]);
          ("architectures", strings [
             "SCMP_ARCH_X86_64"; "SCMP_ARCH_X86"; "SCMP_ARCH_X32" ]);
        ]);
     ]));
  ]

let write ~root bundle_dir t =
  let path = Fpath.(bundle_dir / "config.json") in
  try
    Yojson.Safe.to_file (Fpath.to_string path) (to_yojson ~root t);
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Oci_spec.write %a: %s"
      Fpath.pp path (Printexc.to_string exn)

let placeholder_root = "<rootfs>"

let write_template path t =
  try
    Yojson.Safe.to_file (Fpath.to_string path)
      (to_yojson ~root:placeholder_root t);
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Oci_spec.write_template %a: %s"
      Fpath.pp path (Printexc.to_string exn)
