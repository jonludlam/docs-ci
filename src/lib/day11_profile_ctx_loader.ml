(** Per-tick [Profile_ctx.t] loader.

    [Profile_ctx.load] calls [Git_packages.of_repositories] which
    internally uses [Lwt_main.run] via git-unix. Running that from
    inside OCurrent's Lwt engine (under [Lwt_eio.with_event_loop])
    would nest event loops — undefined behaviour. We dodge that by
    detaching the load into a thread via [Lwt_preemptive.detach], so
    the git-unix code gets its own fresh Lwt world.

    [Profile_ctx.t] isn't marshalable (closures, hashtables), so the
    Op's [Value] is just a digest marker; the actual ctx is stashed
    in a per-profile [ref] and picked up by the caller after the Op
    resolves. Safe because [Current_cache] serialises [build] per-key,
    so the ref update for a given profile never races with itself. *)

module Profile = Day11_batch.Profile
module Profile_ctx = Day11_batch.Profile_ctx

(* Side-channel storage for the non-marshalable [Profile_ctx.t].
   The Op's build writes into [stores] keyed by the profile name;
   the resolving pipeline reads back from it. *)
let stores : (string, Profile_ctx.t) Hashtbl.t = Hashtbl.create 4

(* Per-process nonce — included in [Key.digest] so that every startup
   forces a cache miss, re-runs [build], and repopulates [stores].
   Without this, a sqlite-cached prior success would skip [build] but
   leave [stores] empty. *)
let process_nonce =
  Printf.sprintf "%d-%f" (Unix.getpid ()) (Unix.gettimeofday ())

module Op = struct
  type t = {
    profile : Profile.t;
    cache_dir : Fpath.t;
  }

  module Key = struct
    type t = {
      profile_name : string;
      repos_with_shas : (string * string) list;
    }

    let digest k =
      let sorted =
        List.sort compare
          (List.map (fun (p, s) -> p ^ "@" ^ s) k.repos_with_shas) in
      Digest.to_hex
        (Digest.string
           (String.concat "\n"
              (process_nonce :: k.profile_name :: sorted)))

    let pp f k =
      Fmt.pf f "profile-ctx %s (%d repos)"
        k.profile_name (List.length k.repos_with_shas)
  end

  module Value = struct
    (* The [Profile_ctx.t] isn't marshalable; the Op's value is just
       the key digest so [Current_cache] can detect re-runs. *)
    type t = string
    let marshal = Fun.id
    let unmarshal = Fun.id
  end

  let id = "day11-profile-ctx"
  let auto_cancel = false

  let pp f (key : Key.t) = Key.pp f key

  (* Snapshots live at [~/.day11/snapshots/<profile>/<key>/].
     [cache_dir] is [~/.day11/cache], so we go one up then down
     into [snapshots]. Matches the batch CLI's layout so that the
     [snapshots]/[diff]/[results] commands see docs-ci runs too. *)
  let snapshots_base_for ~cache_dir profile_name =
    Fpath.(parent cache_dir / "snapshots" / profile_name)

  let write_snapshot_and_run ~cache_dir ~profile_name ~repos_with_shas =
    let snapshots_base = snapshots_base_for ~cache_dir profile_name in
    let snapshot =
      let key = Day11_batch.Snapshot.compute_key repos_with_shas in
      let created =
        let t = Unix.gettimeofday () in
        let tm = Unix.gmtime t in
        Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
          (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
      Day11_batch.Snapshot.{ repos = repos_with_shas; key; created }
    in
    let dir = Fpath.(snapshots_base / snapshot.key) in
    ignore (Bos.OS.Dir.create ~path:true dir);
    let _ = Day11_batch.Snapshot.save dir snapshot in
    snapshot

  let build op job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Mostly_harmless in
    Current.Job.log job "[profile %s] loading ctx for %d repos"
      op.profile.name (List.length key.repos_with_shas);
    (* Override the profile's static [opam_repositories] with the
       live paths we've just resolved via Current_git. *)
    let profile =
      { op.profile with
        opam_repositories = List.map fst key.repos_with_shas } in
    let* ctx = Profile_ctx.load_lwt profile ~cache_dir:op.cache_dir in
    Hashtbl.replace stores op.profile.name ctx;
    let snapshot =
      write_snapshot_and_run ~cache_dir:op.cache_dir
        ~profile_name:op.profile.name
        ~repos_with_shas:key.repos_with_shas in
    Current.Job.log job "[profile %s] ctx loaded, snapshot=%s"
      op.profile.name snapshot.key;
    Lwt.return (Ok (Key.digest key))
end

module Cache = Current_cache.Make (Op)

(** Is [s] a URL we should clone via [Current_git]? *)
let is_url s =
  let starts p =
    String.length s >= String.length p
    && String.sub s 0 (String.length p) = p in
  starts "http://" || starts "https://" || starts "git+"
  || starts "git@" || starts "ssh://"

(** A live [(path, sha) Current.t] for one entry of a profile's
    [opam_repositories]. URLs flow through [Current_git.clone] with
    hourly polling; local paths flow through [Current_git.Local] which
    watches the .git dir via inotify. *)
let repo_source ~schedule ~git_local_cache entry : (string * string) Current.t =
  let open Current.Syntax in
  if is_url entry then begin
    let commit = Current_git.clone ~schedule entry in
    let+ c = commit in
    let path = Fpath.to_string (Current_git.Commit.repo c) in
    let sha = Current_git.Commit_id.hash (Current_git.Commit.id c) in
    (path, sha)
  end else begin
    let path = Fpath.v entry in
    let local =
      match Hashtbl.find_opt git_local_cache entry with
      | Some l -> l
      | None ->
        let l = Current_git.Local.v path in
        Hashtbl.replace git_local_cache entry l;
        l in
    let+ commit = Current_git.Local.head_commit local in
    let sha = Current_git.Commit_id.hash (Current_git.Commit.id commit) in
    (entry, sha)
  end

(** Full repos_with_shas [Current.t] for a profile. *)
let repos_with_shas ~schedule ~git_local_cache (profile : Profile.t) =
  let entries =
    List.map (repo_source ~schedule ~git_local_cache)
      profile.opam_repositories in
  Current.list_seq entries

(** The snapshot directory (on disk) for a given
    [(path, sha) list]. Used by callers that want to write
    per-snapshot outputs (status.json, solutions, etc.). *)
let snapshot_dir_of ~cache_dir ~profile_name repos_with_shas =
  let key = Day11_batch.Snapshot.compute_key repos_with_shas in
  Fpath.(Op.snapshots_base_for ~cache_dir profile_name / key)

(** Bundle of everything a profile's sub-pipeline needs per tick.
    Returned by [resolve]. The [ctx] and [snapshot_dir] currents are
    driven by the same underlying [repos_with_shas] polling, so they
    change together on any opam-repo commit. *)
type resolved = {
  ctx : Day11_batch.Profile_ctx.t Current.t;
  snapshot_dir : Fpath.t Current.t;
  repos_with_shas : (string * string) list Current.t;
}

let resolve ~schedule ~git_local_cache ~cache_dir (profile : Profile.t) =
  let open Current.Syntax in
  let op = Op.{ profile; cache_dir } in
  let repos = repos_with_shas ~schedule ~git_local_cache profile in
  let digest =
    Current.component "[%s] profile-ctx" profile.name
    |>
    let> repos_with_shas = repos in
    Cache.get op Op.Key.{ profile_name = profile.name; repos_with_shas } in
  let ctx =
    let+ _digest = digest in
    match Hashtbl.find_opt stores profile.name with
    | Some ctx -> ctx
    | None ->
      failwith (Printf.sprintf "profile %s: ctx store unpopulated"
                  profile.name)
  in
  let snapshot_dir =
    let+ repos_with_shas = repos in
    snapshot_dir_of ~cache_dir ~profile_name:profile.name repos_with_shas
  in
  { ctx; snapshot_dir; repos_with_shas = repos }
