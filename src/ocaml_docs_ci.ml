module Git = Current_git

let setup_log default_level =
  Prometheus_unix.Logging.init ?default_level ();
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  Memtrace.trace_if_requested ~context:"ocaml-docs-ci" ()

let hourly = Current_cache.Schedule.v ~valid_for:(Duration.of_hour 1) ()
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

let main () current_config github_auth mode config : unit =
  (* Pre-compute git_packages BEFORE entering the Eio event loop,
     because git-unix uses Lwt_main.run internally which can't be
     nested inside Lwt_eio's event loop. *)
  let opam_repo = Sys.getenv_opt "DAY11_OPAM_REPO"
    |> Option.value ~default:(Sys.getenv "HOME" ^ "/ocaml/opam-repository") in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories [ (opam_repo, None) ] in
  ignore @@ Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _token ->
  let eio_env = (env :> Eio_unix.Stdenv.base) in
  let repo_opam =
    Git.clone ~schedule:hourly
      "https://github.com/ocaml/opam-repository.git"
  in
  let engine =
    Current.Engine.create ~config:current_config (fun () ->
        Docs_ci_pipelines.Docs.v ~config ~opam:repo_opam
          ~eio_env ~git_packages ~repos_with_shas ()
        |> Current.ignore_value)
  in
  let has_role =
    if github_auth = None then Current_web.Site.allow_all else has_role
  in
  let secure_cookies = github_auth <> None in
  let authn = Option.map Current_github.Auth.make_login_uri github_auth in
  let site =
    let routes =
      Routes.(
        (s "login" /? nil) @--> Current_github.Auth.login github_auth)
      :: Current_web.routes engine
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
      $ Docs_ci_lib.Config.cmdliner)

let () = exit @@ Cmd.eval cmd
