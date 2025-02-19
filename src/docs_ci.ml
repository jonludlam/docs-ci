module Git = Current_git

let () = Logging.init ()

let monthly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) ()

let program_name = "docs-ci"

let main current_config mode gql_port config =
  let repo_opam = Git.clone ~schedule:monthly "https://github.com/ocaml/opam-repository.git" in
  let api = Docs_ci_lib.Web.make config in
  let engine =
    Current.Engine.create ~config:current_config (fun () ->
        Docs_ci_pipelines.Docs.v ~config ~api ~opam:repo_opam () |> Current.ignore_value)
  in
  let site =
    let routes = Current_web.routes engine in
    Current_web.Site.(v ~has_role:allow_all) ~name:program_name routes
  in
  Logging.run
    (Lwt.choose
       [
         Current.Engine.thread engine;
         (* The main thread evaluating the pipeline. *)
         Current_web.run ~mode site;
         (* Optional: provides a web UI *)
         Docs_ci_lib.Web.serve ~port:gql_port api |> Lwt.map Result.ok;
       ])

(* Command-line parsing *)

open Cmdliner

let graphql_port =
  Arg.value @@ Arg.opt Arg.int 8081
  @@ Arg.info
       ~doc:"The port on which to listen for incoming Graphql endpoint HTTP connections."
       ~docv:"GQL_PORT" [ "gql-port" ]

let cmd =
  let doc = "an OCurrent pipeline" in
  ( Term.(
      const main $ Current.Config.cmdliner $ Current_web.cmdliner $ graphql_port
      $ Docs_ci_lib.Config.cmdliner),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
