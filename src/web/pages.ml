(** Dashboard page resources.

    Each page is a {!Current_web.Resource.t} that reads its data
    from the on-disk {!Day11_batch}/{!Day11_lib} layout — no caching,
    no background indexing — and renders via TyXML. The site
    chrome (top nav) is added by [Context.respond_ok].

    Wired up in {!Routes}; that module is the public entry point
    for ocaml-docs-ci to register these pages. *)

open Tyxml.Html
module Resource = Current_web.Resource
module Context = Current_web.Context
module Profile = Day11_batch.Profile

(** Shared per-process context. [profile_dir] is the directory that
    holds the [<name>.json] profile files; [cache_dir] is the day11
    cache root from which snapshot dirs are derived. *)
type ctx = {
  profile_dir : Fpath.t;
  cache_dir : Fpath.t;
}

(** [snapshots_base ctx name] is the on-disk dir holding all
    snapshots for the named profile. Mirrors
    [Day11_profile_ctx_loader.snapshots_base_for]. *)
let snapshots_base ctx name =
  Fpath.(parent ctx.cache_dir / "snapshots" / name)

(** Look up the OCurrent job_id for a given build_hash by querying the
    Current_cache sqlite db. Returns the most recent job_id (highest
    finished timestamp) or [None] if not cached. Accepts either the
    short (12-char) form or the full 32-char hash — we match on the
    [substr(key,1,N)] prefix for whichever length we got. *)
let job_id_for_hash hash =
  let n = min 12 (String.length hash) in
  let prefix = String.sub hash 0 n in
  let db_path = "/home/jjl25/ocaml-docs-ci-oi-sharing/var/db/sqlite.db" in
  if not (Sys.file_exists db_path) then None
  else
    try
      let db = Sqlite3.db_open ~mode:`READONLY db_path in
      let stmt = Sqlite3.prepare db
        "SELECT job_id FROM cache \
         WHERE substr(key,1,?) = ? AND op LIKE 'day11-%' \
         ORDER BY finished DESC LIMIT 1" in
      let _ = Sqlite3.bind_int stmt 1 n in
      let _ = Sqlite3.bind_blob stmt 2 prefix in
      let result = ref None in
      (match Sqlite3.step stmt with
       | Sqlite3.Rc.ROW ->
         (match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> result := Some s
          | _ -> ())
       | _ -> ());
      ignore (Sqlite3.finalize stmt);
      ignore (Sqlite3.db_close db);
      !result
    with _ -> None

(** List a profile's snapshots, newest first by mtime. *)
let list_snapshots_newest_first ctx name =
  let base = snapshots_base ctx name in
  match Bos.OS.Dir.contents base with
  | Error _ -> []
  | Ok entries ->
    entries
    |> List.filter_map (fun p ->
      try
        if Bos.OS.Dir.exists p |> Result.value ~default:false then
          let stat = Unix.stat (Fpath.to_string p) in
          Some (p, stat.Unix.st_mtime)
        else None
      with _ -> None)
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.map fst

(** Read [packages/] under a snapshot dir. *)
let snapshot_packages snapshot_dir =
  let pdir = Fpath.(snapshot_dir / "packages") in
  match Bos.OS.Dir.contents pdir with
  | Error _ -> []
  | Ok entries -> List.map Fpath.basename entries |> List.sort compare

(** Latest status from [packages/<pkg>/history.jsonl] in a snapshot
    dir, or [None] if the file is missing or empty. Reads the LAST
    line of the file (the rolling history is append-only). *)
let latest_pkg_status snapshot_dir pkg_str =
  let h = Fpath.(snapshot_dir / "packages" / pkg_str / "history.jsonl") in
  match Bos.OS.File.read_lines h with
  | Error _ -> None
  | Ok lines ->
    let last = List.fold_left (fun _ l -> l) "" lines in
    if last = "" then None
    else
      try
        let json = Yojson.Safe.from_string last in
        let open Yojson.Safe.Util in
        Some (json |> member "status" |> to_string)
      with _ -> None

(** Latest [(status, category, build_hash)] for a package, or [None]. *)
let latest_pkg_status_full snapshot_dir pkg_str =
  let h = Fpath.(snapshot_dir / "packages" / pkg_str / "history.jsonl") in
  match Bos.OS.File.read_lines h with
  | Error _ -> None
  | Ok lines ->
    let last = List.fold_left (fun _ l -> l) "" lines in
    if last = "" then None
    else
      try
        let json = Yojson.Safe.from_string last in
        let open Yojson.Safe.Util in
        let status = json |> member "status" |> to_string in
        let category =
          try json |> member "category" |> to_string
          with _ -> status in
        let build_hash =
          try json |> member "build_hash" |> to_string
          with _ -> "" in
        Some (status, category, build_hash)
      with _ -> None

(** Find the snapshot key chronologically just before [current_key]
    in this profile, by mtime. Returns [None] if [current_key] is the
    oldest. Used for the "Diff against previous" button on
    {!snapshot_detail}. *)
let find_previous_snapshot_key ctx name current_key =
  let snaps = list_snapshots_newest_first ctx name in
  let keys = List.map Fpath.basename snaps in
  let rec walk = function
    | [] | [_] -> None
    | k :: next :: _ when k = current_key -> Some next
    | _ :: rest -> walk rest
  in
  walk keys

(* ── /profiles ────────────────────────────────────────────────── *)

let profiles_index ~ctx =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! nav_link = Some "Profiles"
    method! private get web_ctx =
      let names = Profile.list ~dir:ctx.profile_dir in
      let row name =
        let snaps = list_snapshots_newest_first ctx name in
        let snap_count = List.length snaps in
        let latest = match snaps with
          | [] -> txt "—"
          | s :: _ ->
            let key = Fpath.basename s in
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/snapshots/%s"
                              name key) ]
              [ Templates.sha_span key ]
        in
        tr [
          td [ a ~a:[ a_href ("/profiles/" ^ name) ] [ txt name ] ];
          td [ txt (string_of_int snap_count) ];
          td [ latest ];
        ]
      in
      Context.respond_ok web_ctx [
        Templates.style_block;
        h2 [ txt "Profiles" ];
        table ~a:[ a_class [ "data" ] ]
          ~thead:(thead [ tr [ th [ txt "Name" ];
                               th [ txt "Snapshots" ];
                               th [ txt "Latest" ] ] ])
          (List.map row names)
      ]
  end

(* ── /profiles/<name> ─────────────────────────────────────────── *)

let profile_dashboard ~ctx name =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let snaps = list_snapshots_newest_first ctx name in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles"; None, name
      ] in
      let body = match snaps with
        | [] -> [ p [ txt "No snapshots yet for this profile." ] ]
        | s :: _ ->
          let key = Fpath.basename s in
          let snapshot_link =
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/snapshots/%s"
                              name key) ]
              [ txt key ] in
          let snapshots_link =
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/snapshots" name) ]
              [ txt (Printf.sprintf "All snapshots (%d)"
                       (List.length snaps)) ] in
          [ p [ txt "Latest snapshot: "; snapshot_link ];
            ul [ li [ snapshots_link ] ] ]
      in
      Context.respond_ok web_ctx
        ([ Templates.style_block; crumbs; h2 [ txt name ] ] @ body)
  end

(* ── /profiles/<name>/snapshots[?page=N] ──────────────────────── *)

let page_size = 25

let snapshots_list ~ctx name =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let req = Context.request web_ctx in
      let uri = Cohttp.Request.uri req in
      let page =
        match Uri.get_query_param uri "page" with
        | Some s -> (try max 1 (int_of_string s) with _ -> 1)
        | None -> 1
      in
      let snaps = list_snapshots_newest_first ctx name in
      let total = List.length snaps in
      let n_pages = max 1 ((total + page_size - 1) / page_size) in
      let page = min page n_pages in
      let start = (page - 1) * page_size in
      let visible = snaps
        |> List.filteri (fun i _ -> i >= start && i < start + page_size) in
      let row dir =
        let key = Fpath.basename dir in
        let mtime =
          try
            let s = Unix.stat (Fpath.to_string dir) in
            let tm = Unix.gmtime s.st_mtime in
            Printf.sprintf "%04d-%02d-%02d %02d:%02d"
              (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
              tm.tm_hour tm.tm_min
          with _ -> "—"
        in
        tr [
          td [ a ~a:[ a_href (Printf.sprintf "/profiles/%s/snapshots/%s"
                                 name key) ]
                 [ Templates.sha_span key ] ];
          td [ txt mtime ];
        ]
      in
      let pager =
        if n_pages <= 1 then []
        else
          let link p label =
            a ~a:[ a_href (Printf.sprintf
                             "/profiles/%s/snapshots?page=%d" name p) ]
              [ txt label ]
          in
          [ div ~a:[ a_class [ "pager" ] ]
              (List.concat [
                (if page > 1 then [ link (page - 1) "‹ Prev"; txt " " ]
                 else []);
                [ txt (Printf.sprintf "Page %d of %d (%d snapshots)"
                         page n_pages total) ];
                (if page < n_pages then [ txt " "; link (page + 1) "Next ›" ]
                 else []);
              ]) ]
      in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        None, "Snapshots";
      ] in
      Context.respond_ok web_ctx
        ([ Templates.style_block; crumbs;
           h2 [ txt (name ^ " — snapshots") ];
           table ~a:[ a_class [ "data" ] ]
             ~thead:(thead [ tr [ th [ txt "Key" ];
                                  th [ txt "Created (UTC)" ] ] ])
             (List.map row visible) ]
         @ pager)
  end

(* ── /profiles/<name>/snapshots/<key> ─────────────────────────── *)

let snapshot_detail ~ctx name key =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let snapshot_dir = Fpath.(snapshots_base ctx name / key) in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        Some (Printf.sprintf "/profiles/%s/snapshots" name), "Snapshots";
        None, key;
      ] in
      let repos =
        match Bos.OS.File.read Fpath.(snapshot_dir / "repos.json") with
        | Error _ -> []
        | Ok s ->
          try
            let json = Yojson.Safe.from_string s in
            let open Yojson.Safe.Util in
            json |> member "repos" |> to_list
            |> List.map (fun r ->
              let path = r |> member "path" |> to_string in
              let commit = r |> member "commit" |> to_string in
              (path, commit))
          with _ -> []
      in
      let repos_table = match repos with
        | [] -> p [ em [ txt "No repos.json on disk." ] ]
        | _ ->
          table ~a:[ a_class [ "data" ] ]
            ~thead:(thead [ tr [ th [ txt "Repo" ]; th [ txt "Commit" ] ] ])
            (List.map (fun (p, c) ->
              tr [ td [ code [ txt p ] ];
                   td [ Templates.sha_span c ] ]) repos)
      in
      let totals =
        match Day11_lib.Status_index.read ~dir:snapshot_dir with
        | None -> [ p [ em [ txt "Status not yet generated for \
                                  this snapshot — run is in \
                                  progress or pre-finish." ] ] ]
        | Some st ->
          let is_doc_cat c =
            c = "doc_success" || c = "doc_failure"
          in
          let partition rows =
            List.partition (fun (c, _) -> is_doc_cat c) rows
          in
          let build_blessed = snd (partition st.blessed_totals) in
          let doc_blessed = fst (partition st.blessed_totals) in
          let build_nonblessed = snd (partition st.non_blessed_totals) in
          let doc_nonblessed = fst (partition st.non_blessed_totals) in
          let breakdown_row label rows =
            let parts = List.map
              (fun (cat, n) -> Printf.sprintf "%s=%d" cat n) rows in
            let total = List.fold_left (fun acc (_, n) -> acc + n) 0 rows in
            let txt_str = match parts with
              | [] -> "0"
              | _ -> Printf.sprintf "%d (%s)" total
                       (String.concat ", " parts) in
            tr [ th [ txt label ]; td [ txt txt_str ] ]
          in
          [ table ~a:[ a_class [ "data" ] ]
              [ breakdown_row "Blessed builds" build_blessed;
                breakdown_row "Non-blessed builds" build_nonblessed;
                breakdown_row "Blessed docs" doc_blessed;
                breakdown_row "Non-blessed docs" doc_nonblessed ];
            p ~a:[ a_class [ "crumbs" ] ]
              [ em [ txt "Builds count compiled package layers; docs \
                          count successful compile+link (or doc-all) \
                          stages. Counts are per build_hash (a package \
                          solved in N universes counts as N). 'Blessed' \
                          means the entry is the chosen primary \
                          universe per package AND the entry was \
                          written in the current run — i.e. it's still \
                          live. Older blessed entries superseded by a \
                          re-solve count as non-blessed." ] ] ]
      in
      let pkgs = snapshot_packages snapshot_dir in
      let pkg_link p =
        match String.index_opt p '.' with
        | None ->
          a ~a:[ a_href (Printf.sprintf "/profiles/%s/p/%s" name p) ]
            [ txt p ]
        | Some i ->
          let n = String.sub p 0 i in
          let v = String.sub p (i + 1) (String.length p - i - 1) in
          a ~a:[ a_href (Printf.sprintf "/profiles/%s/p/%s/%s" name n v) ]
            [ txt p ]
      in
      (* pkg_table is rendered after [dag_data] is available so we
         can list every package from dag.json (not just those with
         per-snapshot history). See further down. *)
      let _ = pkgs in
      let diff_link = match find_previous_snapshot_key ctx name key with
        | None -> []
        | Some prev ->
          [ p [ a ~a:[ a_href (Printf.sprintf
                                 "/profiles/%s/snapshots/%s/diff/%s"
                                 name prev key) ]
                  [ txt "Diff against previous snapshot ("
                  ; Templates.sha_span prev
                  ; txt ")" ] ] ]
      in
      let kind_label : Day11_lib.Dag_marshal.kind -> string = function
        | Build -> "build" | Tool -> "tool" | Compile -> "compile"
        | Doc_all -> "doc-all" | Link -> "link"
      in
      (* Read dag.json + classify once. Drives Failures list, DAG
         overview, and cascade breakdown — all want the same view. *)
      let dag_data =
        match Day11_lib.Dag_marshal.read ~snapshot_dir with
        | Error _ -> None
        | Ok entries ->
          let os_dir =
            match Profile.load ~dir:ctx.profile_dir ~name with
            | Ok profile ->
              Some Fpath.(ctx.cache_dir / Profile.os_dir_name profile)
            | Error _ -> None
          in
          let cascade_table = match os_dir with
            | Some d ->
              Day11_lib.Cascade.classify_from_layers ~os_dir:d entries
            | None ->
              let packages_dir = Fpath.(snapshot_dir / "packages") in
              Day11_lib.Cascade.classify ~packages_dir entries
          in
          Some (entries, cascade_table)
      in
      (* Surface failed nodes prominently. Driven by cascade_table when
         dag.json is present (comprehensive: includes failures cached
         from previous snapshots that didn't re-dispatch); falls back
         to per-snapshot history for older snapshots without dag.json. *)
      let failures_section =
        match dag_data with
        | Some (entries, cascade_table) ->
          let failed = List.filter_map (fun (e : Day11_lib.Dag_marshal.entry) ->
            match Hashtbl.find_opt cascade_table e.hash with
            | Some { Day11_lib.Cascade.status = Failed; _ } -> Some e
            | _ -> None) entries
          in
          (match failed with
           | [] -> [ p [ em [ txt "No failed nodes 🎉" ] ] ]
           | _ ->
             let by_kind = List.fold_left (fun acc (e : Day11_lib.Dag_marshal.entry) ->
               let k = kind_label e.kind in
               let n = try List.assoc k acc with Not_found -> 0 in
               (k, n + 1) :: List.filter (fun (kk, _) -> kk <> k) acc) [] failed in
             let by_kind = List.sort (fun (_, a) (_, b) -> compare b a) by_kind in
             let summary = String.concat ", " (List.map
               (fun (k, n) -> Printf.sprintf "%s=%d" k n) by_kind) in
             let sorted = List.sort (fun (a : Day11_lib.Dag_marshal.entry) b ->
               compare (OpamPackage.to_string a.pkg)
                       (OpamPackage.to_string b.pkg)) failed in
             let row (e : Day11_lib.Dag_marshal.entry) =
               let pkg_cell = pkg_link (OpamPackage.to_string e.pkg) in
               let log_target = match job_id_for_hash e.hash with
                 | Some job_id -> "/job/" ^ job_id
                 | None ->
                   Printf.sprintf "/profiles/%s/builds/%s/log" name e.hash
               in
               tr [ td [ pkg_cell ];
                    td [ txt (kind_label e.kind) ];
                    td [ Templates.sha_span e.hash ];
                    td [ a ~a:[ a_href log_target ] [ txt "log" ] ] ]
             in
             [ p [ txt (Printf.sprintf "%d failed (%s)"
                          (List.length failed) summary) ];
               table ~a:[ a_class [ "data" ] ]
                 ~thead:(thead [ tr [ th [ txt "Package" ];
                                      th [ txt "Kind" ];
                                      th [ txt "Hash" ];
                                      th [ txt "Log" ] ] ])
                 (List.map row sorted) ])
        | None ->
          (* Fallback: legacy per-snapshot history view. *)
          let failures, with_history =
            List.fold_left (fun (fs, wh) pkg_str ->
              match latest_pkg_status_full snapshot_dir pkg_str with
              | Some (status, _, _) when status = "success" -> (fs, wh + 1)
              | Some (status, category, build_hash) ->
                ((pkg_str, status, category, build_hash) :: fs, wh + 1)
              | None -> (fs, wh)) ([], 0) pkgs
          in
          let failures = List.rev failures in
          let total_pkgs = List.length pkgs in
          let no_history = total_pkgs - with_history in
          (match failures with
           | [] when no_history > 0 ->
             [ p [ em [ txt (Printf.sprintf
               "No dag.json and no history yet (%d/%d packages)."
               no_history total_pkgs) ] ] ]
           | [] -> [ p [ em [ txt "No failed packages 🎉" ] ] ]
           | _ ->
             let row (pkg_str, status, category, _build_hash) =
               tr [ td [ pkg_link pkg_str ];
                    td [ Templates.status_span status ];
                    td [ txt category ] ]
             in
             [ table ~a:[ a_class [ "data" ] ]
                 ~thead:(thead [ tr [ th [ txt "Package" ];
                                      th [ txt "Status" ];
                                      th [ txt "Category" ] ] ])
                 (List.map row failures) ])
      in
      (* Comprehensive DAG overview from on-disk layer state, plus a
         per-cascade breakdown when there's something to show. Both
         derived from one [Cascade.classify_from_layers] pass over
         dag.json + [<os_dir>/layer_status.jsonl]. *)
      let overview_section, cascade_section =
        match dag_data with
        | None -> [], []
        | Some (entries, cascade_table) ->
          let bucket_for : Day11_lib.Cascade.status -> string = function
            | Ok -> "ok" | Failed -> "failed"
            | Cascade _ -> "cascade" | Pending -> "pending"
          in
          let counts : (string * string, int) Hashtbl.t =
            Hashtbl.create 32 in
          List.iter (fun (e : Day11_lib.Dag_marshal.entry) ->
            match Hashtbl.find_opt cascade_table e.hash with
            | None -> ()
            | Some r ->
              let k = (kind_label e.kind, bucket_for r.status) in
              let n = try Hashtbl.find counts k with Not_found -> 0 in
              Hashtbl.replace counts k (n + 1)
          ) entries;
          let kinds = ["build"; "tool"; "compile"; "doc-all"; "link"] in
          let buckets = ["ok"; "failed"; "cascade"; "pending"] in
          let overview_rows = List.map (fun k ->
            let cells = List.map (fun b ->
              let n = try Hashtbl.find counts (k, b) with Not_found -> 0 in
              td [ txt (string_of_int n) ]) buckets in
            tr (th [ txt k ] :: cells)) kinds
          in
          let overview =
            [ h3 [ txt "DAG state" ];
              p [ em [ txt "Every planned node classified by on-disk \
                            layer status. Cascade = dispatch skipped \
                            because a dep failed." ] ];
              table ~a:[ a_class [ "data" ] ]
                ~thead:(thead [ tr (th [ txt "Kind" ] ::
                  List.map (fun b -> th [ txt b ]) buckets) ])
                overview_rows ]
          in
          let by_hash : (string, Day11_lib.Dag_marshal.entry) Hashtbl.t =
            Hashtbl.create (List.length entries) in
          List.iter (fun (e : Day11_lib.Dag_marshal.entry) ->
            Hashtbl.replace by_hash e.hash e) entries;
          let cascaded =
            List.filter_map (fun (e : Day11_lib.Dag_marshal.entry) ->
              match Hashtbl.find_opt cascade_table e.hash with
              | Some { Day11_lib.Cascade.status = Cascade src; _ } ->
                Some (e, src)
              | _ -> None
            ) entries
          in
          let cascade =
            if cascaded = [] then []
            else begin
              let row ((e : Day11_lib.Dag_marshal.entry), src) =
                let src_e = Hashtbl.find by_hash src in
                tr [ td [ pkg_link (OpamPackage.to_string e.pkg) ];
                     td [ txt (kind_label e.kind) ];
                     td [ pkg_link (OpamPackage.to_string src_e.pkg) ];
                     td [ txt (kind_label src_e.kind) ] ]
              in
              let by_root = Hashtbl.create 16 in
              List.iter (fun (e, src) ->
                let prev = try Hashtbl.find by_root src
                  with Not_found -> [] in
                Hashtbl.replace by_root src ((e, src) :: prev)
              ) cascaded;
              let groups = Hashtbl.fold (fun root rs acc ->
                (root, rs) :: acc) by_root [] in
              let groups = List.sort (fun (_, a) (_, b) ->
                compare (List.length b) (List.length a)) groups in
              let rows = List.concat_map (fun (_, rs) ->
                let sorted = List.sort
                  (fun ((a : Day11_lib.Dag_marshal.entry), _)
                       ((b : Day11_lib.Dag_marshal.entry), _) ->
                    compare (OpamPackage.to_string a.pkg)
                            (OpamPackage.to_string b.pkg))
                  rs in
                List.map row sorted) groups
              in
              [ h3 [ txt (Printf.sprintf "Cascaded (%d)"
                            (List.length cascaded)) ];
                p [ em [ txt "Nodes that didn't run because an upstream \
                              node failed." ] ];
                table ~a:[ a_class [ "data" ] ]
                  ~thead:(thead [ tr [ th [ txt "Package" ];
                                       th [ txt "Kind" ];
                                       th [ txt "Blocked by" ];
                                       th [ txt "Kind" ] ] ])
                  rows ]
            end
          in
          overview, cascade
      in
      (* Build pkg_table from dag.json (every planned package, sorted)
         with status from cascade_table. Falls back to the legacy
         per-snapshot list when dag.json is missing. *)
      let pkg_count, pkg_table =
        match dag_data with
        | Some (entries, cascade_table) ->
          let by_name :
            (string, Day11_lib.Cascade.status list) Hashtbl.t =
            Hashtbl.create 4096 in
          List.iter (fun (e : Day11_lib.Dag_marshal.entry) ->
            match e.kind with
            | Build ->
              let name = OpamPackage.to_string e.pkg in
              let prev = try Hashtbl.find by_name name
                with Not_found -> [] in
              let st = match Hashtbl.find_opt cascade_table e.hash with
                | Some r -> r.status
                | None -> Day11_lib.Cascade.Pending
              in
              Hashtbl.replace by_name name (st :: prev)
            | _ -> ()
          ) entries;
          let aggregate sts =
            (* Worst-of (Failed > Cascade > Pending > Ok) — surfaces
               problems on packages with multiple universes. *)
            if List.exists (fun s -> s = Day11_lib.Cascade.Failed) sts
            then "failed"
            else if List.exists (function
                | Day11_lib.Cascade.Cascade _ -> true | _ -> false) sts
            then "cascade"
            else if List.exists (fun s -> s = Day11_lib.Cascade.Pending) sts
            then "pending"
            else "ok"
          in
          let names = Hashtbl.fold (fun n _ acc -> n :: acc) by_name [] in
          let names = List.sort compare names in
          let row name =
            let sts = Hashtbl.find by_name name in
            tr [ td [ pkg_link name ];
                 td [ Templates.status_span (aggregate sts) ] ]
          in
          (List.length names,
           table ~a:[ a_class [ "data" ] ]
             ~thead:(thead [ tr [ th [ txt "Package" ];
                                  th [ txt "Status" ] ] ])
             (List.map row names))
        | None ->
          let pkg_row p =
            let status_cell = match latest_pkg_status snapshot_dir p with
              | Some s -> Templates.status_span s
              | None -> em [ txt "—" ]
            in
            tr [ td [ pkg_link p ]; td [ status_cell ] ]
          in
          (List.length pkgs,
           match pkgs with
           | [] -> p [ em [ txt "No packages tracked yet." ] ]
           | _ ->
             table ~a:[ a_class [ "data" ] ]
               ~thead:(thead [ tr [ th [ txt "Package" ];
                                    th [ txt "Status" ] ] ])
               (List.map pkg_row pkgs))
      in
      Context.respond_ok web_ctx ([
        Templates.style_block; crumbs;
        h2 [ txt (name ^ " / "); Templates.sha_span key ];
      ] @ diff_link
        @ overview_section
        @ [ h3 [ txt "Failures" ] ]
        @ failures_section
        @ cascade_section
        @ [
        h3 [ txt "Repos at this snapshot" ];
        repos_table;
        h3 [ txt "Status totals" ];
      ] @ totals @ [
        h3 [ txt (Printf.sprintf "Packages (%d)" pkg_count) ];
        pkg_table;
      ])
  end

(* ── /profiles/<name>/snapshots/<key>/diff/<other> ────────────── *)

(* Diff two snapshots' on-disk package state. Keyed by
   [(name, version)] so that two versions of the same package
   (e.g. [astring.0.8.3] AND [astring.0.8.5] both present in the
   newer snapshot) each get their own row instead of collapsing
   together. The previous version of this code keyed by name only
   and silently lost the second-version-onwards. *)
let snapshot_diff ~ctx name key_old key_new =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let dir_old = Fpath.(snapshots_base ctx name / key_old) in
      let dir_new = Fpath.(snapshots_base ctx name / key_new) in
      let split_pkg pkg_str =
        match String.index_opt pkg_str '.' with
        | None -> None
        | Some i ->
          let n = String.sub pkg_str 0 i in
          let v = String.sub pkg_str (i + 1) (String.length pkg_str - i - 1) in
          Some (n, v)
      in
      let os_dir =
        match Profile.load ~dir:ctx.profile_dir ~name with
        | Ok profile ->
          Some Fpath.(ctx.cache_dir / Profile.os_dir_name profile)
        | Error _ -> None
      in
      (* Per-snapshot [(name, version) → status]. Sources from
         dag.json + layer state so cached nodes still appear (the
         legacy [packages/] dir is only the dispatched-this-run
         subset). Falls back to history when dag.json is missing. *)
      let aggregate (sts : Day11_lib.Cascade.status list) =
        if List.exists (fun s -> s = Day11_lib.Cascade.Failed) sts
        then "failure"
        else if List.exists (function
            | Day11_lib.Cascade.Cascade _ -> true | _ -> false) sts
        then "cascade"
        else if List.exists (fun s -> s = Day11_lib.Cascade.Pending) sts
        then "pending"
        else "success"
      in
      let load_pkgs dir =
        match Day11_lib.Dag_marshal.read ~snapshot_dir:dir, os_dir with
        | Ok entries, Some od ->
          let table = Day11_lib.Cascade.classify_from_layers
            ~os_dir:od entries in
          let by_pkg : (string * string, Day11_lib.Cascade.status list)
            Hashtbl.t = Hashtbl.create 4096 in
          List.iter (fun (e : Day11_lib.Dag_marshal.entry) ->
            match e.kind, split_pkg (OpamPackage.to_string e.pkg) with
            | Build, Some (n, v) ->
              let st = match Hashtbl.find_opt table e.hash with
                | Some r -> r.status
                | None -> Day11_lib.Cascade.Pending
              in
              let prev = try Hashtbl.find by_pkg (n, v)
                with Not_found -> [] in
              Hashtbl.replace by_pkg (n, v) (st :: prev)
            | _ -> ()
          ) entries;
          Hashtbl.fold (fun key sts acc -> (key, aggregate sts) :: acc)
            by_pkg []
        | _ ->
          (* Legacy fallback. *)
          snapshot_packages dir
          |> List.fold_left (fun m pkg ->
            let entries = Day11_lib.History.read
              ~packages_dir:Fpath.(dir / "packages") ~pkg_str:pkg in
            match entries, split_pkg pkg with
            | latest :: _, Some (n, v) ->
              ((n, v), latest.status) :: m
            | _ -> m
          ) []
      in
      let m_old = load_pkgs dir_old in
      let m_new = load_pkgs dir_new in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        Some (Printf.sprintf "/profiles/%s/snapshots" name), "Snapshots";
        Some (Printf.sprintf "/profiles/%s/snapshots/%s" name key_old),
          Templates.short_sha key_old;
        None, "diff " ^ Templates.short_sha key_new;
      ] in
      let rows =
        let keys = List.sort_uniq compare
          (List.map fst m_old @ List.map fst m_new) in
        (* First pass: classify each (name, version) into one of four
           buckets and collect per-name counts so we can collapse
           "removed X + added Y" into a single "changed" row when a
           package name has exactly one of each (typical for latest-only
           profiles where a version bump replaces the prior version). *)
        let classify =
          List.filter_map (fun (n, v) ->
            let s_old = List.assoc_opt (n, v) m_old in
            let s_new = List.assoc_opt (n, v) m_new in
            match s_old, s_new with
            | None, None -> None
            | None, Some sn -> Some (n, `Added (v, sn))
            | Some _, None -> Some (n, `Removed v)
            | Some so, Some sn when so = sn -> None
            | Some so, Some sn -> Some (n, `StatusChanged (v, so, sn))
          ) keys
        in
        let counts : (string, int ref * int ref) Hashtbl.t =
          Hashtbl.create 16 in
        List.iter (fun (n, k) ->
          let r, a = try Hashtbl.find counts n
            with Not_found ->
              let cell = (ref 0, ref 0) in
              Hashtbl.add counts n cell;
              cell
          in
          match k with
          | `Removed _ -> incr r
          | `Added _ -> incr a
          | `StatusChanged _ -> ()
        ) classify;
        (* Pair up removed+added for names with exactly 1 of each. *)
        let paired : (string, string * string * string) Hashtbl.t =
          Hashtbl.create 16 in
        List.iter (fun (n, k) ->
          match Hashtbl.find_opt counts n with
          | Some (r, a) when !r = 1 && !a = 1 ->
            let existing = Hashtbl.find_opt paired n in
            (match k, existing with
             | `Removed v_old, None ->
               Hashtbl.add paired n (v_old, "", "")
             | `Removed v_old, Some (_, v_new, s_new) ->
               Hashtbl.replace paired n (v_old, v_new, s_new)
             | `Added (v_new, s_new), None ->
               Hashtbl.add paired n ("", v_new, s_new)
             | `Added (v_new, s_new), Some (v_old, _, _) ->
               Hashtbl.replace paired n (v_old, v_new, s_new)
             | _ -> ())
          | _ -> ()
        ) classify;
        List.filter_map (fun (n, k) ->
          match k, Hashtbl.find_opt paired n with
          | `Removed _, Some _ -> None  (* collapsed into the added row *)
          | `Added (_, _), Some (v_old, v_new, s_new)
            when v_old <> "" && v_new <> "" ->
            Some (tr [ td [ txt n ];
                       td [ txt (v_old ^ " → " ^ v_new) ];
                       td [ Templates.status_span s_new ];
                       td [ em [ txt "version changed" ] ] ])
          | `Added (v, sn), _ ->
            Some (tr [ td [ txt n ]; td [ txt v ];
                       td [ Templates.status_span sn ];
                       td [ em [ txt "added" ] ] ])
          | `Removed v, _ ->
            Some (tr [ td [ txt n ]; td [ txt v ];
                       td []; td [ em [ txt "removed" ] ] ])
          | `StatusChanged (v, so, sn), _ ->
            Some (tr [ td [ txt n ]; td [ txt v ];
                       td [ Templates.status_span so; txt " → ";
                            Templates.status_span sn ];
                       td [ em [ txt "status changed" ] ] ])
        ) classify
      in
      let body =
        if rows = [] then [ p [ em [ txt "No differences." ] ] ]
        else [ table ~a:[ a_class [ "data" ] ]
                 ~thead:(thead [ tr [ th [ txt "Package" ];
                                      th [ txt "Version" ];
                                      th [ txt "Status" ];
                                      th [ txt "Change" ] ] ])
                 rows ]
      in
      Context.respond_ok web_ctx ([
        Templates.style_block; crumbs;
        h2 [ txt (Printf.sprintf "%s — diff" name) ];
        p [ txt "From "; Templates.sha_span key_old;
            txt " to "; Templates.sha_span key_new ];
      ] @ body)
  end

(* ── /profiles/<name>/p/<pkg> ─────────────────────────────────── *)

let package_index ~ctx name pkg =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      (* Find versions of [pkg] across all snapshots. *)
      let snaps = list_snapshots_newest_first ctx name in
      let versions = List.fold_left (fun acc snap ->
        let pdir = Fpath.(snap / "packages") in
        match Bos.OS.Dir.contents pdir with
        | Error _ -> acc
        | Ok entries ->
          List.fold_left (fun acc p ->
            let basename = Fpath.basename p in
            if String.length basename > String.length pkg + 1
               && String.sub basename 0 (String.length pkg + 1)
                  = pkg ^ "." then
              let v = String.sub basename (String.length pkg + 1)
                (String.length basename - String.length pkg - 1) in
              if List.mem v acc then acc else v :: acc
            else acc) acc entries
      ) [] snaps |> List.sort compare in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        None, "package: " ^ pkg;
      ] in
      let body = match versions with
        | [] -> [ p [ em [ txt "No builds of this package in any \
                                snapshot." ] ] ]
        | _ ->
          [ ul (List.map (fun v ->
              li [ a ~a:[ a_href (Printf.sprintf
                                    "/profiles/%s/p/%s/%s" name pkg v) ]
                     [ txt (pkg ^ "." ^ v) ] ]) versions) ]
      in
      Context.respond_ok web_ctx
        ([ Templates.style_block; crumbs;
           h2 [ txt (name ^ " / " ^ pkg) ] ] @ body)
  end

(* ── /profiles/<name>/p/<pkg>/<ver> ───────────────────────────── *)

let package_version ~ctx name pkg ver =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let pkg_str = pkg ^ "." ^ ver in
      let snaps = list_snapshots_newest_first ctx name in
      let entries = List.concat_map (fun snap ->
        let pdir = Fpath.(snap / "packages") in
        Day11_lib.History.read ~packages_dir:pdir ~pkg_str
      ) snaps in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        Some (Printf.sprintf "/profiles/%s/p/%s" name pkg),
          "package: " ^ pkg;
        None, ver;
      ] in
      let history_rows = List.map (fun (e : Day11_lib.History.entry) ->
        (* Prefer linking to the OCurrent job page (gives a Rebuild
           button and structured log) when we can find it; fall back
           to the raw layer.log file when the cache no longer has the
           job_id (e.g. for entries pre-dating the SQLite cache). *)
        let hash_cell =
          let target = match job_id_for_hash e.build_hash with
            | Some job_id -> "/job/" ^ job_id
            | None ->
              Printf.sprintf "/profiles/%s/builds/%s/log"
                name e.build_hash
          in
          a ~a:[ a_href target ] [ Templates.sha_span e.build_hash ]
        in
        let error_cell = match e.error with
          | Some err -> [ code [ txt err ] ]
          | None -> []
        in
        tr [ td [ txt e.ts ];
             td [ txt e.run ];
             td [ Templates.status_span e.status ];
             td [ txt e.category ];
             td [ hash_cell ];
             td error_cell ]
      ) entries in
      let history_block = match history_rows with
        | [] -> [ p [ em [ txt "No history entries." ] ] ]
        | _ ->
          [ table ~a:[ a_class [ "data" ] ]
              ~thead:(thead [ tr [ th [ txt "Time" ];
                                   th [ txt "Run" ];
                                   th [ txt "Status" ];
                                   th [ txt "Category" ];
                                   th [ txt "Hash" ];
                                   th [ txt "Error" ] ] ])
              history_rows ]
      in
      let docs_link =
        a ~a:[ a_href (Printf.sprintf
                         "/profiles/%s/docs/p/%s/%s/doc/index.html"
                         name pkg ver) ]
          [ txt "Open rendered docs" ]
      in
      Context.respond_ok web_ctx ([
        Templates.style_block; crumbs;
        h2 [ txt pkg_str ];
        p [ docs_link ];
        h3 [ txt "History" ];
      ] @ history_block)
  end

(* ── /profiles/<name>/builds/<hash>/log ──────────────────────── *)

(** Read [layer.log] for a build hash and serve it as text/plain.
    The hash points into the per-arch cache dir (derived from the
    profile's [os_dir_name]). Layer dirs use the first 12 chars of
    the full hash as the directory name; we accept either. Used as
    the link target from the [build_hash] cell of the package
    history table — the answer to "why did this build fail?". *)
let build_log_view ~ctx name hash =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let open Lwt.Syntax in
      let* response =
        match Profile.load ~dir:ctx.profile_dir ~name with
        | Error (`Msg e) ->
          Context.respond_error web_ctx
            `Not_found (Printf.sprintf "no such profile: %s (%s)" name e)
        | Ok profile ->
          let os_dir = Profile.os_dir_name profile in
          let short = if String.length hash <= 12 then hash
                      else String.sub hash 0 12 in
          let log_path = Fpath.(ctx.cache_dir / os_dir / short / "layer.log") in
          (match Bos.OS.File.read log_path with
           | Error _ ->
             Context.respond_error web_ctx
               `Not_found (Printf.sprintf
                  "no log for build %s (looked at %s)"
                  short (Fpath.to_string log_path))
           | Ok body ->
             let headers = Cohttp.Header.init_with "Content-Type"
               "text/plain; charset=utf-8" in
             Cohttp_lwt_unix.Server.respond_string
               ~headers ~status:`OK ~body ())
      in
      Lwt.return response
  end
