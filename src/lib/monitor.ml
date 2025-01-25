module Metrics = struct
  open Prometheus

  let namespace = "docs_ci"
  let subsystem = "monitor"

  let package_status_total =
    let help = "Number of packages by status" in
    Gauge.v_label ~label_name:"status" ~help ~namespace ~subsystem
      "package_status_total"
end

(*TODO: Cut over pipeline_tree's strings to use step_type *)
(* type step_type =
   | Prep
   | DepCompilePrep of OpamPackage.t
   | DepCompileCompile of OpamPackage.t
   | Compile
   | BuildHtml *)

type pipeline_tree =
  | Item : 'a Current.t -> pipeline_tree
  | Seq of (string * pipeline_tree) list
  | And of (string * pipeline_tree) list
  | Or of (string * pipeline_tree) list

type state = Done | Running | Failed [@@deriving show, eq]

type step_status = Err of string | Active | Blocked | OK
[@@deriving show, eq, to_yojson]

type step = { typ : string; job_id : string option; status : step_status }
[@@deriving show, eq, to_yojson]

type steps_list = step list [@@deriving to_yojson]

type package_steps = {
  package : OpamPackage.t;
  status : state;
  steps : step list;
}
[@@deriving eq]

let pp_package ppf r =
  Format.fprintf ppf "%s.%s"
    (OpamPackage.name_to_string r)
    (OpamPackage.version_to_string r)

let pp_package_steps ppf r =
  Format.fprintf ppf "{@ package:@ %a;@ status:@ %a;@ steps:@ @[%a@]@ }"
    pp_package r.package pp_state r.status
    (Format.pp_print_list pp_step)
    r.steps

let opam_package_from_string name =
  OpamPackage.of_string_opt name
  |> Option.to_result ~none:"invalid package name"

type t = {
  mutable solve_failures : string OpamPackage.Map.t;
  mutable preps : pipeline_tree Package.Map.t;
  mutable blessing : Package.Blessing.Set.t Current.t OpamPackage.Map.t;
  mutable trees : pipeline_tree Package.Map.t;
  mutable html : Compile.t Current.t Package.Map.t;
}

let get_blessing t = t.blessing
let get_solve_failures t = t.solve_failures

let make () =
  {
    solve_failures = OpamPackage.Map.empty;
    preps = Package.Map.empty;
    blessing = OpamPackage.Map.empty;
    trees = Package.Map.empty;
    html = Package.Map.empty;
  }

let ( let* ) = Result.bind
let ( let+ ) a f = Result.map f a
let rec simplify = function And [ (_, a) ] -> simplify a | v -> v

let render_level =
  let open Tyxml_html in
  function 0 -> h1 | 1 -> h2 | 2 -> h3 | 3 -> h4 | 4 -> h5 | _ -> h6

let render_list ~bullet ~level ~items ~render =
  let open Tyxml_html in
  ul
    (List.map
       (fun (name, item) ->
         li
           ~a:[ a_style ("list-style-type: " ^ bullet) ]
           [ (render_level level) [ txt name ]; render ~level:(level + 1) item ])
       items)

let rec render ~level =
  let open Tyxml_html in
  function
  | Item current -> (
      let result = Current.observe current in
      let container =
        try
          Current.Analysis.metadata current
          |> Current.observe
          |> Result.to_option
          |> Option.join
          |> function
          | Some { job_id = Some job_id; _ } ->
              fun v -> a ~a:[ a_href ("/job/" ^ job_id) ] [ txt v ]
          | _ -> txt
        with (* if current is not a primitive term *)
        | Failure _ -> txt
      in
      match result with
      | Error (`Msg msg) -> container ("error: " ^ msg)
      | Error (`Active _) -> container "active"
      | Error `Blocked -> container "blocked"
      | Ok _ -> container "OK")
  | Seq items -> render_list ~bullet:"decimal" ~level ~items ~render
  | And items -> render_list ~bullet:"circle" ~level ~items ~render
  | Or items -> render_list ~bullet:"|" ~level ~items ~render

let get_opam_package_info t opam_package =
  let* blessing_current =
    OpamPackage.Map.find_opt opam_package t.blessing
    |> Option.to_result ~none:"couldn't find package"
  in
  let+ blessing_set =
    Current.observe blessing_current |> function
    | Ok v -> Ok v
    | Error _ -> Error "couldn't find blessing set"
  in
  match Package.Blessing.Set.blessed blessing_set with
  | None ->
      let preps = Package.Map.filter (fun package _ ->
        Package.opam package = opam_package) t.preps in
      Or (Package.Map.bindings preps |> List.map (fun (package, tree) ->
          (Package.id package, tree)))
  | Some blessed_package ->
      let blessed_pipeline = Package.Map.find blessed_package t.trees in
      blessed_pipeline

let rec to_steps description arr = function
  | Item current ->
      let status : step_status =
        match Current.observe current with
        | Error (`Msg msg) -> Err msg
        | Error (`Active _) -> Active
        | Error `Blocked -> Blocked
        | Ok _ -> OK
      in
      let job_id =
        try
          Current.Analysis.metadata current
          |> Current.observe
          |> Result.to_option
          |> Option.join
          |> function
          | Some { job_id = Some job_id; _ } -> Some job_id
          | _ -> None
        with Failure _ -> None
      in
      { typ = description; job_id; status } :: arr
  | Seq items | And items | Or items ->
      let f name = Fmt.str "%s" name in
      let f_colon name = ":" ^ Fmt.str "%s" name in
      List.map
        (fun (name, item) ->
          to_steps
            (if description = "" then description ^ f name
             else description ^ f_colon name)
            arr item)
        items
      |> List.flatten

let render_package_state t opam_package =
  let name = OpamPackage.name_to_string opam_package in
  let all = Package.Map.filter_map (fun p x -> if Package.opam p = opam_package then Some x else None) t.preps in
  match OpamPackage.Map.find_opt opam_package t.solve_failures with
  | Some reason ->
      let open Tyxml_html in
      Ok
        [
          h1 [ txt ("Package " ^ name) ];
          h2 [ txt "Failed to find a solution:" ];
          pre [ txt reason ];
        ]
  | None ->
      let* blessed_pipeline = get_opam_package_info t opam_package in
      let open Tyxml_html in
      Ok
        ([
          h1 [ txt ("Package " ^ name) ];
          render ~level:1 (simplify blessed_pipeline);
          h2 [ txt ("All universes")];

        ] @ List.flatten (List.map (fun (pkg, pipeline) ->
          [ h3 [ txt (Package.universe pkg |> Package.Universe.hash) ];
            render ~level:2 (simplify pipeline) ]) (Package.Map.bindings all)))

let handle t ~engine:_ str =
  object
    inherit Current_web.Resource.t
    val! can_get = `Viewer

    method! private get context =
      let response =
        let package = opam_package_from_string str in
        match package with
        | Error msg ->
            Tyxml_html.[ txt "An error occured:"; br (); i [ txt msg ] ]
        | Ok package -> (
            match render_package_state t package with
            | Ok page -> page
            | Error msg ->
                Tyxml_html.[ txt "An error occured:"; br (); i [ txt msg ] ])
      in
      Current_web.Context.respond_ok context response
  end

let max a b =
  match (a, b) with
  | Done, v -> v
  | v, Done -> v
  | _, Failed -> Failed
  | Failed, _ -> Failed
  | Running, Running -> Running

let rec pipeline_state = function
  | Item v -> (
      let result = Current.observe v in
      match result with
      | Ok _ -> Done
      | Error (`Active _) -> Running
      | Error `Blocked -> Running
      | Error (`Msg _) -> Failed)
  | Seq lst | And lst | Or lst ->
      List.fold_left (fun acc (_, v) -> max (pipeline_state v) acc) Done lst

let opam_package_state t opam_package =
  match
    let+ blessed_pipeline = get_opam_package_info t opam_package in
    pipeline_state blessed_pipeline
  with
  | Ok v -> v
  | Error _ -> Failed

let lookup_known_packages t =
  let blessings = get_blessing t |> OpamPackage.Map.keys in
  List.map (fun blessing -> OpamPackage.name_to_string blessing) blessings

let lookup_done t =
  OpamPackage.Map.keys t.blessing
  |> List.map (fun k -> (k, opam_package_state t k))
  |> List.filter (fun (_, st) -> st = Done)

let lookup_failed_pending t =
  OpamPackage.Map.keys t.blessing
  |> List.map (fun k -> (k, opam_package_state t k))
  |> List.filter (fun (_, st) -> st != Done)
  |> List.partition (fun (_, st) -> st == Failed)

let lookup_failed_compiles t =
  let open Tyxml_html in
  let n = ref 0 in
  let x = Package.Map.fold (fun _pkg current acc ->
    let status : step_status =
      match Current.observe current with
      | Error (`Msg msg) -> incr n; Err msg
      | Error (`Active _) -> Active
      | Error `Blocked -> Blocked
      | Ok _ -> OK
    in
    let container =
      try
        Current.Analysis.metadata current
        |> Current.observe
        |> Result.to_option
        |> Option.join
        |> function
        | Some { job_id = Some job_id; _ } ->
            fun v -> a ~a:[ a_href ("/job/" ^ job_id) ] [ txt v ]
        | _ -> txt
      with (* if current is not a primitive term *)
      | Failure _ -> txt
    in
    match status with
    | Err msg -> (container (Printf.sprintf "error: %s" msg)) :: acc
    | _ -> acc)
    t.html [] in
  (x, !n)
     
let lookup_solve_failures t =
  OpamPackage.Map.keys t.solve_failures |> List.map (fun k -> (k, Failed))

let render_link (pkg, _) =
  let open Tyxml_html in
  let name = OpamPackage.to_string pkg in
  li [ a ~a:[ a_href ("/package/" ^ name) ] [ txt name ] ]

let render_pkg ~max_version (pkg_name, versions) =
  let open Tyxml_html in
  let name = OpamPackage.Name.to_string pkg_name in
  li
    [
      txt name;
      ul
        (List.map
           (fun (v, _) ->
             let name =
               OpamPackage.create pkg_name v |> OpamPackage.to_string
             in
             li
               [
                 a
                   ~a:[ a_href ("/package/" ^ name) ]
                   [
                     (if OpamPackage.Version.equal v max_version then
                        b [ txt name ]
                      else txt name);
                   ];
               ])
           versions);
    ]

let group_by_pkg v =
  let by_pkg = ref OpamPackage.Name.Map.empty in
  List.iter
    (fun (p, v) ->
      let name = OpamPackage.name p in
      let ver = OpamPackage.version p in
      match OpamPackage.Name.Map.find_opt name !by_pkg with
      | None -> by_pkg := OpamPackage.Name.Map.add name [ (ver, v) ] !by_pkg
      | Some lst ->
          by_pkg := OpamPackage.Name.Map.add name ((ver, v) :: lst) !by_pkg)
    v;
  !by_pkg

let max_version versions =
  List.fold_left
    (fun max_v (v, _) ->
      if OpamPackage.Version.compare max_v v < 0 then v else max_v)
    (List.hd versions |> fst)
    (List.tl versions)

let map_versions t =
  OpamPackage.Map.keys t.blessing
  |> List.map (fun k -> (k, opam_package_state t k))
  |> group_by_pkg

let map_max_versions t = map_versions t |> OpamPackage.Name.Map.map max_version

let render_passing_packages t =
  let max_version = map_max_versions t in
  let passed = lookup_done t in
  let open Tyxml_html in
  [
    h1 [ txt "Passing packages" ];
    p [ txt ("total: " ^ Int.to_string (List.length passed)) ];
    ul
      (List.map
         (fun (n, v) ->
           render_pkg
             ~max_version:(OpamPackage.Name.Map.find n max_version)
             (n, v))
         (group_by_pkg passed |> OpamPackage.Name.Map.bindings));
  ]

let render_package_root t =
  let max_version = map_max_versions t in
  let failed, pending = lookup_failed_pending t in
  let (failed_do, n) = lookup_failed_compiles t in
  let open Tyxml_html in
  [
    h1 [ txt (Printf.sprintf "Failed 'do' jobs (out of %d checked)" n)];
    ul (List.map (fun x ->
      li [x]) failed_do);
    h1 [ txt "Failed packages" ];
    p [ txt ("total: " ^ Int.to_string (List.length failed)) ];
    ul
      (List.map
         (fun (n, v) ->
           render_pkg
             ~max_version:(OpamPackage.Name.Map.find n max_version)
             (n, v))
         (group_by_pkg failed |> OpamPackage.Name.Map.bindings));
    h1 [ txt "Running packages" ];
    p [ txt ("total: " ^ Int.to_string (List.length pending)) ];
    ul
      (List.map
         (fun (n, v) ->
           render_pkg
             ~max_version:(OpamPackage.Name.Map.find n max_version)
             (n, v))
         (group_by_pkg pending |> OpamPackage.Name.Map.bindings));
    h1 [ txt "Solver failures" ];
    p
      [
        txt
          ("total: "
          ^ Int.to_string
              (List.length @@ OpamPackage.Map.bindings t.solve_failures));
      ];
    ul (List.map render_link (OpamPackage.Map.bindings t.solve_failures));
  ]

let filter_by_name (name : string) :
    (OpamPackage.Name.t * 's) list -> (OpamPackage.Name.t * 's) list =
  List.filter (fun (package_name, _) ->
      OpamPackage.Name.to_string package_name = name)

let lookup_status t ~name =
  let blessings = get_blessing t |> OpamPackage.Map.keys in
  let solve_failures = get_solve_failures t |> OpamPackage.Map.keys in
  let known_projects =
    List.map
      (fun package -> OpamPackage.to_string package)
      (blessings @ solve_failures)
  in
  if not (List.exists (fun x -> x = name) known_projects) then []
    (* we don't know this project *)
  else
    let passed = lookup_done t in
    let passed_packages =
      passed
      |> List.map (fun (package, s) ->
             (OpamPackage.name package, (OpamPackage.version package, s)))
      |> filter_by_name name
      |> List.map (fun (package_name, (package_version, _)) ->
             (package_name, package_version, Done))
    in
    let failed', pending = lookup_failed_pending t in
    let solve_failures = lookup_solve_failures t in
    let failed = solve_failures @ failed' in
    let failed_packages =
      group_by_pkg failed
      |> OpamPackage.Name.Map.bindings
      |> filter_by_name name
      |> List.map (fun (package_name, l) ->
             List.map
               (fun (package_version, _) ->
                 (package_name, package_version, Failed))
               l)
      |> List.flatten
    in
    let pending_packages =
      group_by_pkg pending
      |> OpamPackage.Name.Map.bindings
      |> filter_by_name name
      |> List.map (fun (package_name, l) ->
             List.map
               (fun (package_version, _) ->
                 (package_name, package_version, Running))
               l)
      |> List.flatten
    in
    List.concat [ passed_packages; failed_packages; pending_packages ]

let lookup_steps' t (package : OpamPackage.t) =
  let status = opam_package_state t package in
  let package_pipeline_tree = get_opam_package_info t package in
  let steps = Result.map (fun p -> to_steps "" [] p) package_pipeline_tree in
  Result.map (fun s -> { package; status; steps = s }) steps

let lookup_steps t ~name =
  let blessings = get_blessing t |> OpamPackage.Map.keys in
  let solve_failures = get_solve_failures t |> OpamPackage.Map.keys in
  let packages =
    List.filter
      (fun package -> OpamPackage.name_to_string package = name)
      (blessings @ solve_failures)
  in
  if List.length packages = 0 then
    Error (Fmt.str "no packages found with name: %s" name)
  else
    let r : (package_steps, string) result list =
      List.map (fun package -> lookup_steps' t package) packages
    in

    let errors, oks = List.partition Result.is_error r in

    if List.length errors > 0 then
      let list_errors = List.map Result.get_error errors in
      Error
        (List.fold_left
           (fun acc s -> if acc = "" then acc ^ s else acc ^ " " ^ s)
           "" list_errors)
    else
      let list_packages = List.map Result.get_ok oks in
      Ok list_packages

let collect_metrics t =
  let ok_count = ref 0 in
  let failed_count = ref 0 in
  let running_count = ref 0 in

  let register_status_for_package t name =
    let statuses = lookup_status t ~name in
    List.iter
      (fun (_, _, status) ->
        match status with
        | Done -> ok_count := !ok_count + 1
        | Failed -> failed_count := !failed_count + 1
        | Running -> running_count := !running_count + 1)
      statuses
  in
  let known_packages = lookup_known_packages t in
  List.iter (fun name -> register_status_for_package t name) known_packages;

  (* Environment pipeline package version statuses:
     ok: Package version has built successfully
     failed: Package version build has failed
     running: Package version build is currently running
     solver_failed: Solver failed to find solution for this package version.
  *)
  Prometheus.Gauge.set
    (Metrics.package_status_total "ok")
    (float_of_int !ok_count);
  Prometheus.Gauge.set
    (Metrics.package_status_total "failed")
    (float_of_int !failed_count);
  Prometheus.Gauge.set
    (Metrics.package_status_total "running")
    (float_of_int !running_count);
  Prometheus.Gauge.set
    (Metrics.package_status_total "solver_failed")
    (float_of_int (t.solve_failures |> OpamPackage.Map.keys |> List.length))

let register t solve_failures preps blessing trees html =
  t.solve_failures <- OpamPackage.Map.of_list solve_failures;
  t.preps <- preps;
  t.blessing <- blessing;
  t.trees <- trees;
  t.html <- html

let handle_passing t ~engine:_ =
  object
    inherit Current_web.Resource.t
    method! nav_link = Some "Passing"
    val! can_get = `Viewer

    method! private get context =
      let response = render_passing_packages t in
      Current_web.Context.respond_ok context response
  end

let handle_root t ~engine:_ =
  object
    inherit Current_web.Resource.t
    method! nav_link = Some "Packages"
    val! can_get = `Viewer

    method! private get context =
      let response = render_package_root t in
      Current_web.Context.respond_ok context response
  end

let do_lookups t =
  let failed, pending = lookup_failed_pending t in
  let passed = lookup_done t in
  Prometheus.Gauge.set
    (Metrics.package_status_total "ok")
    (float_of_int (List.length passed));
  Prometheus.Gauge.set
    (Metrics.package_status_total "failed")
    (float_of_int (List.length failed));
  Prometheus.Gauge.set
    (Metrics.package_status_total "running")
    (float_of_int (List.length pending));
  Prometheus.Gauge.set
    (Metrics.package_status_total "solver_failed")
    (float_of_int (t.solve_failures |> OpamPackage.Map.keys |> List.length))

(* Override the OCurrent metrics to add ocaml-docs-ci specific data. *)
let handle_metrics t ~engine =
  object
    inherit Current_web.Resource.t
    val! can_get = `Monitor

    method! private get _context =
      let open Lwt in
      do_lookups t;
      Current.Engine.(update_metrics engine);
      Prometheus.CollectorRegistry.(collect default) >>= fun data ->
      let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
      let headers =
        Cohttp.Header.init_with "Content-Type" "text/plain; version=0.0.4"
      in
      Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()
  end

let routes t engine =
  Routes.
    [
      (s "package" / str /? nil) @--> handle t ~engine;
      (s "package" /? nil) @--> handle_root t ~engine;
      (s "passing" /? nil) @--> handle_passing t ~engine;
      (s "metrics" /? nil) @--> handle_metrics t ~engine;
    ]
