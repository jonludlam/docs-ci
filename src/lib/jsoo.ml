type jsoo_hash = string [@@deriving yojson]

type t = { package : Package.t; jsoo_hash : jsoo_hash }

let jsoo_hash t = t.jsoo_hash

let package t = t.package

let spec ~generation ~ssh ~base ~voodoo prep =
  let open Obuilder_spec in
  let package = Prep.package prep in
  let prep_folder = Storage.folder Prep package in
  let base_jsoo = Storage.Base.folder (Jsoo generation) in
  let jsoo_folder = Storage.folder (Jsoo generation) package in
  let toplevel_by_digest_folder = Storage.toplevel_by_digest_folder (Jsoo generation) in
  let tools = Voodoo.Prep.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         (* Import voodoo-prep *)
         copy ~from:(`Build "tools")
           [ "/home/opam/voodoo-prep" ]
           ~dst:"/home/opam/";
         run ~network:Misc.network "opam pin add -n js_of_ocaml git+https://github.com/jonludlam/js_of_ocaml#lazy_files";
         run ~network:Misc.network "opam pin add -n js_of_ocaml-compiler git+https://github.com/jonludlam/js_of_ocaml#lazy_files";
         run ~network:Misc.network "opam pin add -n js_of_ocaml-toplevel git+https://github.com/jonludlam/js_of_ocaml#lazy_files";
         run ~network:Misc.network "opam install js_of_ocaml-toplevel";
         (* obtain the compiled dependencies, prep folder and extract it *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "rsync -aR %s:%s/./%s ." (Config.Ssh.host ssh)
                  (Config.Ssh.storage_folder ssh) (Fpath.to_string prep_folder);
                Fmt.str "find . -name '*.tar' -exec tar -xvf {} \\;";
              ];
         (* Run voodoo-prep && tar compile/linked output *)
         run "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-prep jsoo";
              ];
         (* Extract compile output   - cache needs to be invalidated if we want to be able to read the logs *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "echo '%f'" (Random.float 1.);
                Fmt.str "mkdir -p %a" Fpath.pp base_jsoo;
                Fmt.str "mv jsoo/* ./%a || true" Fpath.pp base_jsoo;
                Fmt.str "mkdir -p %a" Fpath.pp jsoo_folder;
                Fmt.str "rsync -aR ./%s %s:%s/." (Fpath.to_string jsoo_folder)
                  (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh);
                Fmt.str "rsync -aR ./%s %s:%s/. || true" (Fpath.to_string toplevel_by_digest_folder)
                  (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh);
                Fmt.str "set '%s'; %s" (Fpath.to_string jsoo_folder)
                  (Storage.Tar.hash_command ~prefix:"JSOO" ());
              ];
       ]


let or_default a = function None -> a | b -> b

module Compile = struct
  type output = t

  type t = { generation : Epoch.t; }

  let id = "voodoo-jsoo"

  module Value = struct
    type t = jsoo_hash [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  module Key = struct
    type t = {
      prep : Prep.t;
      voodoo : Voodoo.Prep.t;
      config : Config.t;
    }

    let key { prep; voodoo; _ } =
      Fmt.str "v9-%s-%s-%s"
        (Prep.package prep |> Package.digest)
        (Prep.hash prep)
        (Voodoo.Prep.digest voodoo)

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ prep; _ } = Fmt.pf f "Voodoo do %a" Package.pp (Prep.package prep)

  let auto_cancel = true

  let build { generation } job Key.{ prep; voodoo; config } =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let package = Prep.package prep in
    let base = Misc.get_base_image package in
    let** spec =
      match Prep.result prep with
      | Success ->
          Lwt.return_ok
            (spec ~generation ~ssh:(Config.ssh config) ~voodoo ~base prep)
      | Failed ->
          Lwt.return_error (`Msg "error")
    in
    let action = Misc.to_ocluster_submission spec in
    let version = Misc.base_image_version package in
    let cache_hint = "docs-universe-compile-" ^ version in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action ~cache_hint
        ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
        (Config.ocluster_connection_do config)
    in
    let* build_job = Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    let** _ = Current_ocluster.Connection.run_job ~job build_job in
    let extract_hashes v_compile line =
      (* some early stopping could be done here *)
      let jsoo = Storage.parse_hash ~prefix:"JSOO" line |> or_default v_compile in
      jsoo
    in
    let** hash = Misc.fold_logs build_job extract_hashes None in
    try
      let hash = Option.get hash in
      Lwt.return_ok hash.hash
    with Invalid_argument _ -> Lwt.return_error (`Msg "Compile: failed to parse output")
end

module CompileCache = Current_cache.Make (Compile)

let v ~generation ~config ~name ~voodoo ~deps prep =
  let open Current.Syntax in
  
  Current.component "jsoo %s" name
  |> let> prep = prep
     and> generation = generation
     and> _deps = deps (* Just forcing ordering *)
     and> voodoo = voodoo in
     let package = Prep.package prep in
     let output =
       CompileCache.get
         { Compile.generation }
         Compile.Key.{ prep; voodoo; config }
     in
     Current.Primitive.map_result (Result.map (fun jsoo_hash -> { package; jsoo_hash })) output
