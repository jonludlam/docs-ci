open Cmdliner

let random =
  In_channel.with_open_bin "/dev/urandom" (fun ch ->
      let str =
        String.init 4 (fun _ -> In_channel.input_byte ch |> Option.get |> Char.chr)
      in
      Base64.encode_exn str)

module Ssh = struct
  type t = {
    host : string;
    user : string;
    port : int;
    private_key : string;
    private_key_file : string;
    public_key : string;
    folder : string;
  }

  let named f = Cmdliner.Term.(app (const f))

  let ssh_host =
    Arg.value
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH storage server host (optional for day11 mode)" ~docv:"HOST" [ "ssh-host" ]
    |> named (fun x -> `SSH_host x)

  let ssh_user =
    Arg.value
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH storage server user" ~docv:"USER" [ "ssh-user" ]
    |> named (fun x -> `SSH_user x)

  let ssh_port =
    Arg.value
    @@ Arg.opt Arg.(some int) (Some 22)
    @@ Arg.info ~doc:"SSH storage server port" ~docv:"PORT" [ "ssh-port" ]
    |> named (fun x -> `SSH_port x)

  let ssh_privkey =
    Arg.value
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH private key file" ~docv:"FILE" [ "ssh-privkey" ]
    |> named (fun x -> `SSH_privkey x)

  let ssh_pubkey =
    Arg.value
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH public key file" ~docv:"FILE" [ "ssh-pubkey" ]
    |> named (fun x -> `SSH_pubkey x)

  let ssh_folder =
    Arg.value
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH storage folder" ~docv:"FILE" [ "ssh-folder" ]
    |> named (fun x -> `SSH_folder x)

  let load_file path =
    try
      let ch = open_in path in
      let len = in_channel_length ch in
      let data = really_input_string ch len in
      close_in ch;
      data
    with ex ->
      if Sys.file_exists path then
        failwith @@ Fmt.str "Error loading %S: %a" path Fmt.exn ex
      else failwith @@ Fmt.str "File %S does not exist" path

  let v (`SSH_host host) (`SSH_user user) (`SSH_port port) (`SSH_pubkey pubkey)
      (`SSH_privkey privkey) (`SSH_folder folder) =
    match host with
    | None -> None
    | Some host ->
      let user = Option.value ~default:"opam" user in
      let port = Option.value ~default:22 port in
      let privkey = Option.value ~default:"/dev/null" privkey in
      let pubkey = Option.value ~default:"/dev/null" pubkey in
      let folder = Option.value ~default:"/tmp" folder in
      Some {
        host;
        user;
        port;
        private_key = (try load_file privkey with _ -> "");
        private_key_file =
          Fpath.(
            (Bos.OS.Dir.current () |> Result.get_ok)
            // (of_string privkey |> Result.get_ok)
            |> to_string);
        public_key = (try load_file pubkey with _ -> "");
        folder;
      }

  let cmdliner =
    Term.(
      const v
      $ ssh_host
      $ ssh_user
      $ ssh_port
      $ ssh_pubkey
      $ ssh_privkey
      $ ssh_folder)

  let config t =
    Fmt.str
      {|ControlMaster auto
        Host %s
          IdentityFile ~/.ssh/id_rsa
          Port %d
          User %s
          StrictHostKeyChecking=no
          ControlPath ~/.ssh/master-%%r@%%h:%%p
          GlobalKnownHostsFile=/dev/null
          UserKnownHostsFile=/dev/null
          ConnectTimeout=10
          ConnectionAttempts=100
        |}
      t.host t.port t.user

  let secrets =
    Obuilder_spec.Secret.
      [
        v ~target:"/home/opam/.ssh/id_rsa" "ssh_privkey";
        v ~target:"/home/opam/.ssh/id_rsa.pub" "ssh_pubkey";
        v ~target:"/home/opam/.ssh/config" "ssh_config";
      ]

  let secrets_values t =
    [
      ("ssh_privkey", t.private_key);
      ("ssh_pubkey", t.public_key);
      ("ssh_config", config t);
    ]

  let storage_folder t = t.folder
  let host t = t.host
  let user t = t.user
  let priv_key_file t = Fpath.v t.private_key_file
  let port t = t.port

  let digest t =
    Fmt.str "%s-%s-%d-%s" t.host t.user t.port t.folder
    |> Digest.string
    |> Digest.to_hex
end

type t = {
  jobs : int;
  track_packages : string list;
  take_n_last_versions : int option;
  ocluster_connection_prep : Current_ocluster.Connection.t option;
  ocluster_connection_do : Current_ocluster.Connection.t option;
  ocluster_connection_gen : Current_ocluster.Connection.t option;
  cache_threshold : int;
  valid_packages_path : string;
  ssh : Ssh.t option;
}

let cap_file =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Ocluster capability file (optional for day11 mode)" ~docv:"FILE"
       [ "ocluster-submission" ]

let jobs =
  Arg.required
  @@ Arg.opt Arg.(some int) (Some 8)
  @@ Arg.info ~doc:"Number of parallel jobs on the host machine (for solver)"
       ~docv:"JOBS" [ "jobs"; "j" ]

let track_packages =
  Arg.value
  @@ Arg.opt Arg.(list string) []
  @@ Arg.info ~doc:"Filter the name of packages to track. " ~docv:"PKGS"
       [ "filter" ]

let take_n_last_versions =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"Limit the number of versions" ~docv:"LIMIT" [ "limit" ]

let cache_threshold =
  Arg.value
  @@ Arg.opt Arg.(int) 10
  @@ Arg.info ~doc:"Cache threshold" ~docv:"THRESHOLD" [ "cache-threshold" ]

let valid_packages_path =
  Arg.value
  @@ Arg.opt Arg.(string) "valid_packages.txt"
  @@ Arg.info ~doc:"Valid packages path" ~docv:"PATH" [ "valid-packages" ]

let v cap_file jobs track_packages take_n_last_versions ssh cache_threshold
    valid_packages_path =
  let ocluster =
    match cap_file with
    | Some path ->
      let vat = Capnp_rpc_unix.client_only_vat () in
      let cap = Capnp_rpc_unix.Cap_file.load vat path |> Result.get_ok in
      Some (Current_ocluster.Connection.create ~max_pipeline:100 cap)
    | None -> None
  in

  {
    jobs;
    track_packages;
    take_n_last_versions;
    ocluster_connection_prep = ocluster;
    ocluster_connection_do = ocluster;
    ocluster_connection_gen = ocluster;
    valid_packages_path;
    ssh;
    cache_threshold;
  }

let cmdliner =
  Term.(
    const v
    $ cap_file
    $ jobs
    $ track_packages
    $ take_n_last_versions
    $ Ssh.cmdliner
    $ cache_threshold
    $ valid_packages_path)

(* odoc really pinned to 3.1.0 release now *)
let odoc _ =
  "https://github.com/ocaml/odoc.git#dbe1333c687102e1a9a1cbe59d63b8f69e6b1af0"

let sherlodoc _ = odoc ()
let pool _ = "docs-pipeline"
let jobs t = t.jobs
let track_packages t = t.track_packages
let take_n_last_versions t = t.take_n_last_versions
let ocluster_connection_do t = t.ocluster_connection_do
let ocluster_connection_prep t = t.ocluster_connection_prep
let ocluster_connection_gen t = t.ocluster_connection_gen
let ssh t = t.ssh
let cache_threshold t = t.cache_threshold
let valid_packages_path t = t.valid_packages_path
