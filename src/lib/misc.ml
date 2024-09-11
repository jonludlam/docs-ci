(* module Metrics = struct *)
(*   open Prometheus *)

(*   let namespace = "docs_ci" *)
(*   let subsystem = "docker" *)

(*   let docker_peek_events = *)
(*     let help = "Incoming docker peek events" in *)
(*     Gauge.v ~help ~namespace ~subsystem "peek_events" *)
(* end *)

module Docker = Current_docker.Default

module Platform : sig
  val v : packages:Package.t list -> Ocaml_version.t option
  val to_string : Ocaml_version.t -> string
end = struct
  let v ~packages =
    let ( let* ) = Option.bind in
    let ocaml_version name =
      packages
      |> List.find_opt (fun pkg ->
             pkg |> Package.opam |> OpamPackage.name_to_string = name)
      |> Option.map (fun pkg ->
             pkg |> Package.opam |> OpamPackage.version_to_string)
    in
    let is_base =
      List.exists
        (fun p ->
          Package.opam p |> OpamPackage.name_to_string = "ocaml-base-compiler")
        packages
    in
    let* version =
      if is_base then ocaml_version "ocaml-base-compiler"
      else ocaml_version "ocaml-variants"
    in
    Ocaml_version.of_string version |> Result.to_option

  let to_string v = Ocaml_version.to_string v
end

let tag ocaml_version =
  let minor =
    if Ocaml_version.major ocaml_version >= 5 then
      Fmt.str "%d" (Ocaml_version.minor ocaml_version)
    else Fmt.str "%02d" (Ocaml_version.minor ocaml_version)
  in
  Fmt.str "debian-12-ocaml-%d.%s%s"
    (Ocaml_version.major ocaml_version)
    minor
    (match Ocaml_version.extra ocaml_version with
    | None -> ""
    | Some x -> "-" ^ x |> String.map (function '+' -> '-' | x -> x))

let cache_hint package =
  let packages = Package.all_deps package in
  Platform.v ~packages
  |> Option.value ~default:Ocaml_version.Releases.latest
  |> Platform.to_string

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

module ImgCache = Map.Make(String)
(** Select base image to use *)
let get_base_image =
  let cache = ref ImgCache.empty in
  fun package ->
  let open Current.Syntax in
  let version = Package.ocaml_version package in
  let version_str = Ocaml_version.to_string version in
  (* let* image = Docker.pull ~label:(tag version) ~schedule:weekly ~arch:"amd64" "ocaml/opam" in *)
  match ImgCache.find_opt version_str !cache with
  | Some x -> x
  | None ->
    let result =
      let+ tag =
        Docker.peek ~schedule:weekly ~arch:"amd64" ("ocaml/opam:" ^ tag version)
      in
      (* TODO Include comment on which image this is?
        Resolves to something like:
        `ocaml/opam@sha256:04a0b3ee7288fb3aa7e608c2ccbbbaa289c1810f57265365dd849fc5cc46d9ed`

        debian-12-ocaml-%d.%s%s
      *)
      Spec.make tag
    in
    cache := ImgCache.add version_str result !cache;
    result

let default_base_image =
  let open Current.Syntax in
  let version = Ocaml_version.Releases.latest in
  let+ tag =
    Docker.peek ~schedule:weekly ~arch:"amd64" ("ocaml/opam:" ^ tag version)
  in
  Spec.make tag

let spec_of_job job =
  let install = job.Jobs.install in
  try get_base_image install
  with e ->
    Format.eprintf "Error with job: %a" Package.pp install;
    raise e

let network = [ "host" ]
let docs_cache_folder = "/home/opam/docs-cache/"
let cache = [ Obuilder_spec.Cache.v ~target:docs_cache_folder "ci-docs" ]

(** Obuilder operation to locally pull the selected folders. The [digests]
    option is used to invalidate the operation if the expected value changes. *)
let rsync_pull ~ssh ?(digest = "") folders =
  let sources =
    List.map
      (fun folder ->
        Fmt.str "%s:%s/./%a" (Config.Ssh.host ssh)
          (Config.Ssh.storage_folder ssh)
          Fpath.pp folder)
      folders
    |> String.concat " "
  in
  let cache_sources =
    List.map (Fmt.str "%s./%a" docs_cache_folder Fpath.pp) folders
    |> String.concat " "
  in
  match folders with
  | [] -> Obuilder_spec.comment "no sources to pull"
  | _ ->
      Obuilder_spec.run ~secrets:Config.Ssh.secrets ~cache ~network
        "rsync --delete -avzR %s %s  && rsync -aR %s ./ && echo 'pulled: %s'"
        sources docs_cache_folder cache_sources digest

module LatchedBuilder (B : Current_cache.S.BUILDER) = struct
  module Adaptor = struct
    type t = B.t

    let id = B.id

    module Key = Current.String
    module Value = B.Key
    module Outcome = B.Value

    let run op job _ key = B.build op job key
    let pp f (_, key) = B.pp f key
    let auto_cancel = B.auto_cancel
    let latched = true
  end

  include Current_cache.Generic (Adaptor)

  let get ~opkey ?schedule ctx key = run ?schedule ctx opkey key
end

let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some "docker" -> `Docker
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

let to_obuilder_job build_spec =
  Fmt.to_to_string Obuilder_spec.pp (build_spec |> Spec.finish)

let to_docker_job build_spec =
  let spec_str =
    Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix
      (build_spec |> Spec.finish)
  in
  `Contents spec_str

let to_ocluster_submission spec =
  match profile with
  | `Production | `Dev ->
      to_obuilder_job spec |> Cluster_api.Submission.obuilder_build
  | `Docker -> to_docker_job spec |> Cluster_api.Submission.docker_build

let fold_logs build_job fn =
  (* TODO: what if we encounter an infinitely long line ? *)
  let open Lwt.Syntax in
  let rec aux start next_lines acc =
    match next_lines with
    | ([] | [ _ ]) as e -> (
        let prev_line = match e with [] -> "" | e :: _ -> e in
        let* logs = Cluster_api.Job.log build_job start in
        match (logs, prev_line) with
        | Error (`Capnp e), _ ->
            Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
        | Ok ("", _), "" -> Lwt_result.return acc
        | Ok ("", _), last_line -> aux start [ last_line; "" ] acc
        | Ok (data, next), prev_line ->
            let lines = String.split_on_char '\n' data in
            let fst = List.hd lines in
            let rest = List.tl lines in
            aux next ((prev_line ^ fst) :: rest) acc)
    | line :: next -> aux start next (fn acc line)
  in
  aux 0L []

let tar_cmd folder =
  let f = Fpath.to_string folder in
  Fmt.str
    "shopt -s nullglob && ((echo LISTING %s && [ ! -z \"$(ls -A %s)\" ] && tar -cvf %s.tar %s/*  && rm -R %s/* && mv %s.tar \
     %s/content.tar) || (echo 'Empty directory'))"
    f f f f f f f

module Cmd = struct
  let tar = tar_cmd

  let list =
    let open Fmt in
    to_to_string (list ~sep:(const string " && ") (fun f -> pf f "(%s)"))

  let list_list ?(max=1024*1024) l =
    match l with
    | [] -> []
      | _ ->
      let (cmd, split) =
        List.fold_right (fun x (str, acc) ->
          if String.length str + String.length x > max then
            ((Printf.sprintf "(%s)" x), str :: acc)
          else
            (Printf.sprintf "(%s) && %s" x str, acc)
        ) l (Printf.sprintf "(%s)" (List.hd l), [])
      in
      cmd :: split
end
