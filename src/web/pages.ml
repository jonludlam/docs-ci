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
          let tot label rows =
            let total = List.fold_left (fun acc (_, n) -> acc + n) 0 rows in
            tr [ th [ txt label ]; td [ txt (string_of_int total) ] ]
          in
          [ table ~a:[ a_class [ "data" ] ]
              [ tot "Blessed total" st.blessed_totals;
                tot "Non-blessed total" st.non_blessed_totals ] ]
      in
      let pkgs = snapshot_packages snapshot_dir in
      let pkg_count = List.length pkgs in
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
      let pkg_list = match pkgs with
        | [] -> p [ em [ txt "No packages tracked yet." ] ]
        | _ ->
          ul (List.map (fun p -> li [ pkg_link p ])
                (if pkg_count <= 200 then pkgs
                 else List.filteri (fun i _ -> i < 200) pkgs
                      @ [ Printf.sprintf
                            "... (%d more, truncated)"
                            (pkg_count - 200) ]))
      in
      Context.respond_ok web_ctx ([
        Templates.style_block; crumbs;
        h2 [ txt (name ^ " / "); Templates.sha_span key ];
        h3 [ txt "Repos at this snapshot" ];
        repos_table;
        h3 [ txt "Status totals" ];
      ] @ totals @ [
        h3 [ txt (Printf.sprintf "Packages (%d)" pkg_count) ];
        pkg_list;
      ])
  end

(* ── /profiles/<name>/snapshots/<key>/diff/<other> ────────────── *)

(* Mirrors [day11 diff]'s logic in cmd_diff: read both snapshots'
   per-package latest history entries and compute version changes /
   added / removed / fixed / regressed. *)
let snapshot_diff ~ctx name key_old key_new =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let dir_old = Fpath.(snapshots_base ctx name / key_old) in
      let dir_new = Fpath.(snapshots_base ctx name / key_new) in
      let load_pkgs dir =
        snapshot_packages dir
        |> List.fold_left (fun m pkg ->
          let entries = Day11_lib.History.read
            ~packages_dir:Fpath.(dir / "packages") ~pkg_str:pkg in
          match entries with
          | [] -> m
          | latest :: _ ->
            (* Strip [.version] to get the bare name → version map. *)
            (match String.index_opt pkg '.' with
             | None -> m
             | Some i ->
               let n = String.sub pkg 0 i in
               let v = String.sub pkg (i + 1) (String.length pkg - i - 1) in
               (n, (v, latest.status)) :: m)
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
        let names = List.sort_uniq compare
          (List.map fst m_old @ List.map fst m_new) in
        List.filter_map (fun n ->
          let v_old = List.assoc_opt n m_old in
          let v_new = List.assoc_opt n m_new in
          match v_old, v_new with
          | None, None -> None
          | None, Some (vn, sn) ->
            Some (tr [ td [ txt n ]; td [ txt "—" ];
                       td [ txt vn ]; td [ Templates.status_span sn ];
                       td [ em [ txt "added" ] ] ])
          | Some (vo, _), None ->
            Some (tr [ td [ txt n ]; td [ txt vo ]; td [ txt "—" ];
                       td []; td [ em [ txt "removed" ] ] ])
          | Some (vo, so), Some (vn, sn) when vo = vn && so = sn -> None
          | Some (vo, so), Some (vn, sn) ->
            let kind =
              if vo <> vn then Printf.sprintf "version: %s → %s" vo vn
              else "status changed"
            in
            Some (tr [ td [ txt n ]; td [ txt vo ]; td [ txt vn ];
                       td [ Templates.status_span so; txt " → ";
                            Templates.status_span sn ];
                       td [ em [ txt kind ] ] ])
        ) names
      in
      let body =
        if rows = [] then [ p [ em [ txt "No differences." ] ] ]
        else [ table ~a:[ a_class [ "data" ] ]
                 ~thead:(thead [ tr [ th [ txt "Package" ];
                                      th [ txt "Old version" ];
                                      th [ txt "New version" ];
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
        tr [ td [ txt e.ts ];
             td [ txt e.run ];
             td [ Templates.status_span e.status ];
             td [ txt e.category ];
             td [ Templates.sha_span e.build_hash ];
             td [ txt e.compiler ];
             td (match e.error with
                 | None -> []
                 | Some err -> [ code [ txt err ] ]) ]
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
                                   th [ txt "Compiler" ];
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
