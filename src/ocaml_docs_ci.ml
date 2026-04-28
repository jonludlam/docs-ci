module Profile = Day11_batch.Profile

let setup_log default_level =
  Prometheus_unix.Logging.init ?default_level ();
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  Memtrace.trace_if_requested ~context:"ocaml-docs-ci" ()

let program_name = "ocaml-docs-ci"

let has_role user = function
  | `Viewer | `Monitor -> true
  | _ -> (
      match Option.map Current_web.User.id user with
      | Some
          ( "github:talex5" | "github:avsm" | "github:kit-ty-kate"
          | "github:samoht" | "github:tmcgilchrist" | "github:dra27"
          | "github:jonludlam" | "github:TheLortex" | "github:sabine"
          | "github:mtelvers" | "github:shonfeder" ) ->
          true
      | _ -> false)

(* Load profile JSON only. The heavy [Profile_ctx.t] construction
   (git_packages / hash_cache) is deferred to a per-tick OCurrent Op
   in [Day11_profile_ctx_loader], so changes to any opam repository
   (remote or local) flow through the pipeline and invalidate the
   solver cache. *)
let load_profiles ~profile_dir names : Day11_batch.Profile.t list =
  List.map (fun name ->
    match Profile.load ~dir:profile_dir ~name with
    | Error (`Msg e) ->
      Fmt.epr "error loading profile %s: %s@." name e;
      exit 2
    | Ok profile ->
      if profile.opam_repositories = [] then begin
        Fmt.epr "profile %s has empty opam_repositories@." name;
        exit 2
      end;
      profile
  ) names

let main () current_config github_auth mode profiles_arg profile_dir_arg
    cache_dir_arg remotes_arg cores_per_build overcommit config : unit =
  let profile_dir =
    Fpath.v (match profile_dir_arg with
      | Some d -> d
      | None ->
        Filename.concat (Sys.getenv "HOME") ".day11/profiles")
  in
  let cache_dir =
    Fpath.v (match cache_dir_arg with
      | Some d -> d
      | None ->
        Filename.concat (Sys.getenv "HOME") ".day11/cache")
  in
  ignore (Bos.OS.Dir.create ~path:true cache_dir);
  let remote_specs =
    List.map (fun arg ->
      match Docs_ci_lib.Remote_opam_repo.spec_of_arg arg with
      | Ok s -> s
      | Error (`Msg e) ->
        Fmt.epr "error: %s@." e;
        exit 2
    ) remotes_arg
  in
  if remote_specs <> [] then
    Logs.app (fun f -> f "Maintaining %d remote opam repo%s"
      (List.length remote_specs)
      (if List.length remote_specs = 1 then "" else "s"));
  let names =
    match profiles_arg with
    | [] ->
      (* Default: every profile in [profile_dir]. *)
      Profile.list ~dir:profile_dir
    | ns -> ns
  in
  if names = [] then begin
    Fmt.epr "no profiles in %a and none named on the command line@."
      Fpath.pp profile_dir;
    exit 2
  end;
  Logs.app (fun f -> f "Loading %d profile%s: %s"
    (List.length names) (if List.length names = 1 then "" else "s")
    (String.concat ", " names));
  let profiles = load_profiles ~profile_dir names in
  (* Optional NUMA-aware cpu slot pool. When configured, every
     container launch acquires a slot with a pinned cpuset +
     NUMA-local memory node, capping nested build parallelism
     via cgroup v2. *)
  let cpu_slots = match cores_per_build with
    | None | Some 0 -> None
    | Some n ->
      let pool =
        Day11_runner.Cpu_slots.auto ~cores_per_build:n ~overcommit () in
      Logs.app (fun f -> f "CPU pool: %s"
        (Day11_runner.Cpu_slots.describe pool));
      Some pool
  in
  ignore @@ Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _token ->
  let eio_env = (env :> Eio_unix.Stdenv.base) in
  (* Map from a profile's [opam_repositories] entry (a local path) to
     a live [Current_git.Commit.t Current.t] driven by
     [Remote_opam_repo.maintain_commit]. The pipeline reads HEAD
     directly from this map — no inotify, no filesystem watcher in
     between. A profile entry whose path isn't backed by a [--remote]
     spec falls back to a one-shot read of HEAD at startup. *)
  let remote_schedule =
    Current_cache.Schedule.v ~valid_for:(Duration.of_hour 1) () in
  let remote_commits :
    (string, Current_git.Commit.t Current.t) Hashtbl.t =
    Hashtbl.create (List.length remote_specs) in
  List.iter (fun (s : Docs_ci_lib.Remote_opam_repo.spec) ->
    let commit = Docs_ci_lib.Remote_opam_repo.maintain_commit
      ~schedule:remote_schedule ~url:s.url ~path:s.path in
    Hashtbl.replace remote_commits (Fpath.to_string s.path) commit
  ) remote_specs;
  let engine =
    Current.Engine.create ~config:current_config (fun () ->
      Docs_ci_pipelines.Docs.v ~config
        ~eio_env ~cache_dir ~profiles ~remote_commits ?cpu_slots ()
      |> Current.ignore_value)
  in
  let has_role =
    if github_auth = None then Current_web.Site.allow_all else has_role
  in
  let secure_cookies = github_auth <> None in
  let authn = Option.map Current_github.Auth.make_login_uri github_auth in
  let site =
    let dashboard_routes =
      Docs_ci_web.Web_routes.routes
        ~ctx:{ profile_dir; cache_dir } in
    let routes =
      Routes.[
        (s "login" /? nil) @--> Current_github.Auth.login github_auth;
      ]
      @ dashboard_routes
      @ Current_web.routes engine
    in
    Current_web.Site.(v ?authn ~has_role ~secure_cookies)
      ~name:program_name routes
  in
  Lwt_eio.Promise.await_lwt (Lwt.choose
    [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ])

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(const setup_log $ Logs_cli.level ~docs ())

let profiles_arg =
  Arg.value
  @@ Arg.opt Arg.(list string) []
  @@ Arg.info ~doc:"Comma-separated list of day11 profile names to run. \
                    If unset, every profile in --profile-dir is used."
       ~docv:"PROFILES" [ "profiles" ]

let profile_dir_arg =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Directory containing day11 profile JSON files. \
                    Defaults to ~/.day11/profiles."
       ~docv:"DIR" [ "profile-dir" ]

let cache_dir_arg =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"day11 cache root. Defaults to ~/.day11/cache."
       ~docv:"DIR" [ "cache-dir" ]

let remotes_arg =
  Arg.value
  @@ Arg.opt_all Arg.string []
  @@ Arg.info
       ~doc:"Mirror a remote opam-repository into a local path. \
             Repeatable. Format: $(b,URL=PATH). ocaml-docs-ci clones \
             $(b,URL) into $(b,PATH) at startup and fetches it \
             hourly; local commits are preserved (fast-forward-only \
             merge, fails if the working tree has diverged). Day11 \
             profiles reference $(b,PATH) as a regular local repo."
       ~docv:"URL=PATH" [ "remote" ]

let cores_per_build_arg =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info
       ~doc:"Cores per container. Enables cgroup cpuset pinning and \
             NUMA-local memory allocation (when the host has 2+ NUMA \
             nodes). Host CPUs are split into slots of this size; \
             each container sees exactly N cpus via [nproc]. 0 / \
             unset disables pinning."
       ~docv:"N" [ "cores-per-build" ]

let overcommit_arg =
  Arg.value
  @@ Arg.opt Arg.float 1.0
  @@ Arg.info
       ~doc:"Multiplier on the strict CPU-bounded slot count. 1.0 \
             (default) gives each build exclusive cpus; 1.5 shares \
             cpusets 50% of the time; 2.0 doubles every cpuset. Only \
             effective when --cores-per-build is set."
       ~docv:"FACTOR" [ "overcommit" ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "OCaml documentation CI pipeline" in
  let info = Cmd.info program_name ~doc ~version in
  Cmd.v info
    Term.(
      const main
      $ setup_log
      $ Current.Config.cmdliner
      $ Current_github.Auth.cmdliner
      $ Current_web.cmdliner
      $ profiles_arg
      $ profile_dir_arg
      $ cache_dir_arg
      $ remotes_arg
      $ cores_per_build_arg
      $ overcommit_arg
      $ Docs_ci_lib.Config.cmdliner)

let () = exit @@ Cmd.eval cmd
