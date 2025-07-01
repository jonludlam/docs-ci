(* Valid packages *)

module Op = struct
  type t = Config.t

  module Key = struct
    type t = Fpath.t
    let digest = Fpath.to_string
  end

  module Value = struct
    type t = Package.Set.t
    let digest v =
      Package.Set.elements v
      |> List.map Package.to_yojson
      |> fun l -> Yojson.Safe.to_string (`List l)
      |> Digest.string
  end

  module Outcome = Current.Unit

  let id = "publish-valid-packages"

  let pp f (k, _v) = Fmt.pf f "Publish valid packages to path %a" Fpath.pp k
  let auto_cancel = true
  let publish config job fpath packages =
    let open Lwt.Syntax in
    let ssh = Config.ssh config in
    let* () = Current.Job.start ~level:Harmless job in
    let path = Fpath.to_string fpath in
    let () = Out_channel.with_open_bin (Printf.sprintf "%s.tmp" path) (fun oc ->
      Package.Set.iter (fun p -> 
        let opam = Package.opam p in
        Printf.fprintf oc "u/%s/%s/%s\n%!" (Package.universe p |> Package.Universe.hash) (OpamPackage.name_to_string opam) (OpamPackage.version_to_string opam);
      ) packages) in
    let () = Unix.rename (Printf.sprintf "%s.tmp" path) path in
    let cmd = Bos.Cmd.(
        v "scp" % "-P" % Int.to_string (Config.Ssh.port ssh)
          % "-i"
          % p (Config.Ssh.priv_key_file ssh)
          % Config.valid_packages_path config
          % (Config.Ssh.user ssh ^ "@" ^ Config.Ssh.host ssh ^ ":" ^ path)) in
    Current.Process.exec ~cancellable:true ~job ("", Bos.Cmd.to_list cmd |> Array.of_list)
end

module Publish = Current_cache.Output (Op)


let set_current config solver_result_c path v =
  let open Current.Syntax in
  Current.component "Publish valid packages"
  |> 
     let> _solver_result_c = solver_result_c in
     Publish.set config path v