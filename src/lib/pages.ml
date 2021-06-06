(* Pages - /packages/index.html and packages/<foo>/index.html *)

let id = "pages"

let spec ~ssh ~base ~cache_key ~voodoo () =
  let open Obuilder_spec in
  let tools = Voodoo.Do.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-do"; "/home/opam/voodoo-gen" ]
           ~dst:"/home/opam/";
         run "echo hello";
         Git_store.Cluster.clone ~branch:"status" ~directory:"git-store" ssh;
         workdir "git-store";
         run "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-gen packages -o html";
         run "git add --all";
         run "git commit -m 'docs ci update status\n\n%s' --allow-empty" cache_key;
         Git_store.Cluster.push ssh;
       ]

module Pages = struct
  type t = { voodoo : Voodoo.Do.t; config : Config.t }

  let id = "update-pages"

  let auto_cancel = true

  module Key = struct
    type t = string

    let digest v = Format.asprintf "pages-%s" v
  end

  module Value = struct
    type t = Current_git.Commit.t

    let digest = Fmt.to_to_string Current_git.Commit.pp
  end

  module Outcome = Current.Unit

  let pp fmt (_k, v) = Format.fprintf fmt "metadata-%a" Current_git.Commit.pp v

  let publish { voodoo; config } job _ _v =
    Current.Job.log job "Publish pages";
    let open Lwt.Syntax in
    let base = Spec.make "ocaml/opam:ubuntu-ocaml-4.12" in
    let spec = spec ~ssh:(Config.ssh config) ~base ~cache_key:"foo" ~voodoo () in
    let action = Misc.to_ocluster_submission spec in
    let version = "4.12" in
    let cache_hint = "docs-universe-compile-" ^ version in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action ~cache_hint
        ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
        (Config.ocluster_connection_do config)
    in
    let* build_job = Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    let* result = Current_ocluster.Connection.run_job ~job build_job in
    match result with Error (`Msg _) as e -> Lwt.return e | Ok _ -> Lwt.return (Ok ())
end

module PagesCache = Current_cache.Output (Pages)

let v ~config ~voodoo ~commit =
  let open Current.Syntax in
  Current.component "publish-pages"
  |> let> voodoo = voodoo and> commit = commit in
     PagesCache.set { config; voodoo } "pages" commit
