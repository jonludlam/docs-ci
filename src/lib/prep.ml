let prep_version = "v3"
let network = Misc.network
let cache = Voodoo.cache

module OpamPackage = struct
  include OpamPackage

  let to_yojson t = `String (OpamPackage.to_string t)

  let of_yojson = function
    | `String str -> (
        match OpamPackage.of_string_opt str with
        | Some x -> Ok x
        | None -> Error "failed to parse version")
    | _ -> Error "failed to parse version"
end
module OpamFiles = struct

  type t = No_context

  let id = "opam-files"

  let auto_cancel = true

  module Key = struct
    type t = { repo : Current_git.Commit.t; packages : OpamPackage.t list }
  
    let digest { repo; packages } =
      Current_git.Commit.hash repo ^ String.concat ";" (List.map OpamPackage.to_string packages)
  end

  module Value = struct
    type t = (OpamPackage.t * (bool * string)) list
    [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string
    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok

  end

  let pool = Current.Pool.create ~label:"git_pool" 5


  
let build _ job { Key.repo; packages } =
  let package_path pkg =
    Fpath.(v "packages" / (OpamPackage.name pkg |> OpamPackage.Name.to_string) / OpamPackage.to_string pkg)
  in
  let open Lwt.Infix in
  Current.Job.start ~pool ~level:Harmless job >>= fun () ->
  Current_git.with_checkout ~job repo (fun path ->
    Current.Process.with_tmpdir ~prefix:"opam-files" (fun fp ->
      let open Lwt.Infix in
      let r = Lwt_list.map_s (fun pkg ->
        let open Lwt.Syntax in
        let tarfile = Fpath.(fp / (Fmt.str "%s-opam.tar.bz2" (OpamPackage.to_string pkg))) in
        let has_depext =
          match Bos.OS.File.read Fpath.(path // package_path pkg / "opam") with
          | Ok content ->
            let f = OpamFile.OPAM.read_from_string content in
            OpamFile.OPAM.depexts f <> []
          | _ -> false
        in
        let command =
          Bos.Cmd.(
            v "tar"
            % "-C"
            % Fpath.to_string path
            % "-jcf"
            % Fpath.to_string tarfile
            % (Fpath.to_string (package_path pkg) )
          )
        in
        Current.Job.log job "About to exec";
        let* res = Current.Process.exec ~cancellable:true ~job
        ("", Bos.Cmd.to_list command |> Array.of_list) in
        Current.Job.log job "Finished";

        match res with
      | Ok () -> begin
        let res = Bos.OS.File.read tarfile in
        match res with
        | Ok contents -> Lwt.return_ok (pkg, (has_depext, Base64.encode_string contents))
        | Error (`Msg m) -> Lwt.return_error (`Msg m)
        end
      | Error (`Msg m) -> Lwt.return_error (`Msg m)
      ) packages in
      r >>= fun pkgs ->
      Lwt.return (Ok (List.filter_map (function | Ok x -> Some x | _ -> None) pkgs))
      
      ))

  let pp f { Key.repo; packages } =
    Fmt.pf f "opamfiles\n%a\n%a" Current_git.Commit.pp_short repo
      (Fmt.list (Fmt.of_to_string OpamPackage.to_string)) packages

end

module type F = Current_cache.S.BUILDER

module OpamFilesCache = Current_cache.Make (OpamFiles)

let not_base x =
  not
    (List.mem
       (OpamPackage.name_to_string x)
       [
         "base-unix";
         "base-bigarray";
         "base-threads";
         "ocaml-config";
         "ocaml";
         "ocaml-base-compiler";
         "ocaml-variants";
       ])

let add_base ocaml_version init =
  let add_one x pkgs =
    if List.mem x pkgs then pkgs else x :: pkgs
  in
  let mk n v =
    OpamPackage.create
      (OpamPackage.Name.of_string n)
      (OpamPackage.Version.of_string v)
  in
  List.fold_right add_one [
    mk "base-unix" "base";
    mk "base-bigarray" "base";
    mk "base-threads" "base";
    mk "ocaml-base-compiler" ocaml_version;
    mk "ocaml" ocaml_version;
  ] init

(* association list from package to universes encoded as "<PKG>:<UNIVERSE HASH>,..."
   to be consumed by voodoo-prep *)
let universes_assoc packages =
  packages
  |> List.map (fun pkg ->
         let hash = pkg |> Package.universe |> Package.Universe.hash in
         let name = pkg |> Package.opam |> OpamPackage.name_to_string in
         name ^ ":" ^ hash)
  |> String.concat ","

let spec ~ssh ~voodoo ~base ~(install : Package.t) ~opamfiles (prep : Package.t list) =
  let open Obuilder_spec in
  (* the list of packages to install (which is a superset of the packages to prep) *)
  let all_deps = Package.all_deps install in

  let ocaml_version = Package.ocaml_version install in

  let packages_str_list =
    all_deps
    |> Package.topo_sort
    |> List.map Package.opam
    |> List.filter not_base
  in

  (* Only enable dune cache for dune >= 2.1 - to remove errors like:
     #=== ERROR while compiling base64.2.3.0 =======================================#
     # context              2.0.8 | linux/x86_64 | ocaml-base-compiler.4.06.1 | file:///src
     # path                 ~/.opam/4.06/.opam-switch/build/base64.2.3.0
     # command              ~/.opam/4.06/bin/dune build -p base64 -j 127
     # exit-code            1
     # env-file             ~/.opam/log/base64-1-9efc19.env
     # output-file          ~/.opam/log/base64-1-9efc19.out
     ### output ###
     # Error: link: /home/opam/.cache/dune/db/v2/temp/promoting: Invalid cross-device link
  *)
  let dune_cache_enabled =
    all_deps
    |> List.exists (fun p ->
           let opam = Package.opam p in
           let min_dune_version = OpamPackage.Version.of_string "2.1.0" in
           match OpamPackage.name_to_string opam with
           | "dune" ->
               OpamPackage.Version.compare (OpamPackage.version opam)
                 min_dune_version
               >= 0
           | _ -> false)
  in

  let prep_storage_folders = List.rev_map (fun p -> (Storage.Prep, p)) prep in

  let create_dir_and_copy_logs_if_not_exist =
    let command =
      Fmt.str
        "([ -d $1 ] || (echo \"FAILED:$2\" && mkdir -p $1 && cp ~/opam.err.log \
         $1 && opam show $3 --raw > $1/opam)) && (%s)"
        (Misc.tar_cmd (Fpath.v "$1"))
    in
    Storage.for_all prep_storage_folders command
  in

  let install_opamfiles tar_b64 =
    run "(echo \"%s\" | base64 -d | sudo tar -jxvC /src)" tar_b64
  in
  let tools = Voodoo.Prep.spec ~base voodoo |> Spec.finish in
  base
  |> Spec.children ~name:"tools" tools
  |> Spec.add
       ([
         (* Install required packages *)
         run "sudo mkdir /src";
         run "echo b3BhbS12ZXJzaW9uOiAiMi4wIgpicm93c2U6ICJodHRwczovL29wYW0ub2NhbWwub3JnL3BrZy8iCnVwc3RyZWFtOiAiaHR0cHM6Ly9naXRodWIuY29tL29jYW1sL29wYW0tcmVwb3NpdG9yeS90cmVlL21hc3Rlci8iCmFubm91bmNlOiBbCiIiIgpbV0FSTklOR10gb3BhbSBpcyBvdXQtb2YtZGF0ZS4gUGxlYXNlIGNvbnNpZGVyIHVwZGF0aW5nIGl0IChodHRwczovL29wYW0ub2NhbWwub3JnL2RvYy9JbnN0YWxsLmh0bWwpCiIiIiB7KG9wYW0tdmVyc2lvbiA+PSAiMi4xLjB+fiIgJiBvcGFtLXZlcnNpb24gPCAiMi4xLjUiKSB8IG9wYW0tdmVyc2lvbiA8ICIyLjAuMTAifQoiIiIKW0lORk9dIG9wYW0gMi4xIGluY2x1ZGVzIG1hbnkgcGVyZm9ybWFuY2UgaW1wcm92ZW1lbnRzIG92ZXIgMi4wOyBwbGVhc2UgY29uc2lkZXIgdXBncmFkaW5nIChodHRwczovL29wYW0ub2NhbWwub3JnL2RvYy9JbnN0YWxsLmh0bWwpCiIiIiB7b3BhbS12ZXJzaW9uID49ICIyLjAuMTAiICYgb3BhbS12ZXJzaW9uIDwgIjIuMS4wfn4ifQpdCg== | base64 -d | sudo tee /src/repo";
         run "ls -lR /src";
         (* Re-initialise opam after switching from opam.2.0 to 2.1. *)
         run ~network
           "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam repo remove default --all && opam init --reinit \
            -ni";
         run "sudo mkdir /src/packages";
         run "opam repo add opam /src";
         copy ~from:(`Build "tools")
           [ "/home/opam/voodoo-prep" ]
           ~dst:"/home/opam/";
         (* Enable build cache conditionally on dune version *)
         env "DUNE_CACHE" (if dune_cache_enabled then "enabled" else "disabled");
         env "DUNE_CACHE_TRANSPORT" "direct";
         env "DUNE_CACHE_DUPLICATION" "copy";
       ] @ 
         List.flatten (List.map (fun pkg ->
          match List.assoc_opt pkg opamfiles with
          | Some (_has_depext, opamfiles) ->
              [ install_opamfiles opamfiles ]
          | _ -> []) (add_base ocaml_version [])
           ) @
         (* Intall packages. Recover in case of failure. *)
         List.flatten (List.map (fun pkg ->
            match List.assoc_opt pkg opamfiles with
            | Some (has_depext, opamfiles) ->
              [ install_opamfiles opamfiles;
                run ~network ~cache "(opam update && opam install -vv --debug-level=2 %s --confirm-level=unsafe-yes --solver=builtin-0install %s 2>&1 && opam clean -s | tee ~/opam.err.log) || echo \
                      'Failed to install all packages'" (if has_depext then "" else "--no-depexts") (OpamPackage.to_string pkg)
              ]
            | None -> [ run "echo Failed to find opamfiles for %s" (OpamPackage.to_string pkg) ]) packages_str_list)
        @ [
        (* Perform the prep step for all packages *)
         run "opam exec -- ~/voodoo-prep -u %s" (universes_assoc prep);
         (* Extract artifacts  - cache needs to be invalidated if we want to be able to read the logs *)
         ] @
          
                (List.map (run ~network ~secrets:Config.Ssh.secrets "%s")
                  (create_dir_and_copy_logs_if_not_exist))
          @ 

                (* Extract *)
                (List.map (run ~network ~secrets:Config.Ssh.secrets "%s") (Storage.for_all prep_storage_folders
                  (Fmt.str "rsync -aR --no-p ./$1 %s:%s/.;"
                     (Config.Ssh.host ssh)
                     (Config.Ssh.storage_folder ssh))))
          @ (List.map (run ~network ~secrets:Config.Ssh.secrets "%s")
                (* Compute hashes *)
                (Storage.for_all prep_storage_folders
                  (Storage.Tar.hash_command ~prefix:"HASHES" ()));
       ))

module Prep = struct
  type t = No_context

  let id = "voodoo-prep"
  let auto_cancel = true

  module Key = struct
    type t = {
      job : Jobs.t;
      base : Spec.t;
      voodoo : Voodoo.Prep.t;
      config : Config.t;
      opamfiles : (OpamPackage.t * (bool * string)) list
    }

    let digest { job = { install; prep }; voodoo; base = _; config = _; opamfiles = _ } =
      (* base is derived from 'prep' so we don't need to include it in the hash *)
      Fmt.str "%s\n%s\n%s\n%s" prep_version (Package.digest install)
        (Voodoo.Prep.digest voodoo)
        (String.concat "\n"
           (List.rev_map Package.digest prep |> List.sort String.compare))
      |> Digest.string
      |> Digest.to_hex
  end

  let pp f Key.{ job = { install; _ }; _ } =
    Fmt.pf f "Voodoo prep %a" Package.pp install

  module Value = struct
    type item = Storage.id_hash [@@deriving yojson]
    type t = item list * string list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string
    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let build No_context job Key.{ job = { install; prep }; base; voodoo; config; opamfiles }
      =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    (* Problem: no rebuild if the opam definition changes without affecting the universe hash.
       Should be fixed by adding the oldest opam-repository commit in the universe hash, but that
       requires changes in the solver.
       For now we rebuild only if voodoo-prep changes.
    *)
    let spec = spec ~ssh:(Config.ssh config) ~voodoo ~base ~install ~opamfiles prep in
    let action = Misc.to_ocluster_submission spec in
    let version = Misc.cache_hint install in
    Current.Job.log job "Prep job: install %a" Package.pp install;
    Current.Job.log job "Prep job: prep %a" (Fmt.list Package.pp) prep;

    let cache_hint = "docs-universe-prep-" ^ version in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action
        ~cache_hint
        ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
        (Config.ocluster_connection_prep config)
    in
    let* build_job =
      Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job
    in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.write job
      (Fmt.str
         "@.To reproduce locally:@.@.cat > prep.spec \
          <<'END-OF-SPEC'@.\o033[34m%s\o033[0m@.END-OF-SPEC@.@.ocluster-client \
          submit-obuilder --local-file prep.spec \\@.--pool linux-x86_64 \
          --connect ocluster-submission.cap --cache-hint %s \\@.--secret \
          ssh_privkey:id_rsa --secret ssh_pubkey:id_rsa.pub--secret \
          ssh_config:ssh_config@.@."
         (Spec.to_spec spec)
         cache_hint);

    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    (* extract result from logs *)
    let extract_hashes ((git_hashes, failed), retriable_errors) line =
      let retry_conditions log_line =
        let retry_on =
          [
            "Temporary failure";
            "Could not resolve host";
            "rsync: connection unexpectedly closed";
            "Disconnected: Switch turned off";
          ]
        in
        List.fold_left
          (fun acc str -> acc || Astring.String.is_infix ~affix:str log_line)
          false retry_on
      in
      let escape_on_success log_line =
        let escape_on = [ "Job succeeded" ] in
        List.fold_left
          (fun acc str -> acc || Astring.String.is_infix ~affix:str log_line)
          false escape_on
      in
      match Storage.parse_hash ~prefix:"HASHES" line with
      | Some value -> ((value :: git_hashes, failed), retriable_errors)
      | None -> (
          if escape_on_success line then ((git_hashes, failed), [])
            (* ignore retriable errors if the job has succeeded *)
          else if retry_conditions line then
            ((git_hashes, failed), line :: retriable_errors)
          else
            match String.split_on_char ':' line with
            | [ prev; branch ]
              when Astring.String.is_suffix ~affix:"FAILED" prev ->
                Current.Job.log job "Failed: %s" branch;
                ((git_hashes, branch :: failed), retriable_errors)
            | _ -> ((git_hashes, failed), retriable_errors))
    in

    let fn () =
      let** _ = Current_ocluster.Connection.run_job ~job build_job in
      Misc.fold_logs build_job extract_hashes (([], []), [])
    in

    let** git_hashes, failed =
      Retry.retry_loop ~job ~log_string:(Current.Job.id job) fn
    in
    Lwt.return_ok
      ( List.map
          (fun (r : Storage.id_hash) ->
            Current.Job.log job "%s -> %s" r.id r.hash;
            r)
          git_hashes,
        failed )
end

module PrepCache = Current_cache.Make (Prep)

type prep_result = Success | Failed

type t = {
  base : Spec.t;
  hash : string;
  package : Package.t;
  result : prep_result;
}

let hash t = t.hash
let package t = t.package
let result t = t.result
let base t = t.base

type prep = t Package.Map.t

let pp f t = Fmt.pf f "%s:%s" (Package.id t.package) t.hash

let compare a b =
  match String.compare a.hash b.hash with
  | 0 -> Package.compare a.package b.package
  | v -> v

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let combine ~base ~(job : Jobs.t) (artifacts_branches_output, failed_branches) =
  let packages = job.prep in
  let artifacts_branches_output =
    artifacts_branches_output
    |> List.to_seq
    |> Seq.map (fun Storage.{ id; hash } -> (id, hash))
    |> StringMap.of_seq
  in
  let failed_branches = StringSet.of_list failed_branches in
  packages
  |> List.to_seq
  |> Seq.filter_map (fun package ->
         let package_id = Package.id package in
         match StringMap.find_opt package_id artifacts_branches_output with
         | Some hash when StringSet.mem package_id failed_branches ->
             Some (package, { base; package; hash; result = Failed })
         | Some hash -> Some (package, { base; package; hash; result = Success })
         | None -> None)
  |> Package.Map.of_seq

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let v ~config ~voodoo ~spec (job : Jobs.t) =
  let open Current.Syntax in
  let repo_opam =
    Current_git.clone ~schedule:weekly
      "https://github.com/ocaml/opam-repository.git"
  in
  let ocaml_version = Package.ocaml_version job.install in

  let opamfiles = 
    Current.component "opamfiles %s" (job.install |> Package.digest) |>
    let> repo_opam in
    let packages = Package.all_deps job.install |> List.map Package.opam in
    let packages = add_base ocaml_version packages in
    OpamFilesCache.get No_context OpamFiles.Key.{ repo = repo_opam; packages }
  in
  Current.component "voodoo-prep %s" (job.install |> Package.digest)
  |> let> voodoo and> spec and> opamfiles in
     PrepCache.get No_context { job; voodoo; config; base = spec; opamfiles }
     |> Current.Primitive.map_result (Result.map (combine ~base:spec ~job))

let extract ~(job : Jobs.t) (prep : prep Current.t) =
  let open Current.Syntax in
  List.map
    (fun package ->
      ( package,
        let+ prep in
        Package.Map.find package prep ))
    job.prep
  |> List.to_seq
  |> Package.Map.of_seq
