(** Bridge between day11's build/doc primitives and OCurrent's caching.

    This module provides OCurrent-compatible wrappers around day11's
    stateless build and doc primitives, mapping layer hashes to
    OCurrent cache digests. *)

(* ── Types ─────────────────────────────────────────────────────── *)

type build_env = {
  env : Eio_unix.Stdenv.base;
  benv : Day11_opam_build.Types.build_env;
  os_dir : Fpath.t;
  cache_dir : Fpath.t;
  opam_repositories : Fpath.t list;
}

type doc_env = {
  build : build_env;
  driver_tool : Day11_opam_layer.Tool.t;
  odoc_tool : Day11_opam_layer.Tool.t;
  blessed : bool;
}

(* ── Helpers ───────────────────────────────────────────────────── *)

let layer_exists os_dir hash =
  let dir = Day11_layer.Dir.path ~os_dir hash in
  Bos.OS.File.exists Fpath.(dir / "layer.json") |> Result.get_ok

let layer_dir os_dir hash =
  Day11_layer.Dir.path ~os_dir hash

(* ── Build package (replaces prep) ─────────────────────────────── *)

module Build = struct
  type t = {
    pkg : OpamPackage.t;
    hash : string;  (* deterministic layer hash *)
  }

  let pkg t = t.pkg
  let hash t = t.hash
  let layer_dir ~os_dir t = Day11_layer.Dir.path ~os_dir t.hash

  (** Check if a build layer exists on disk. *)
  let is_cached ~os_dir t = layer_exists os_dir t.hash

  (** Build a package, returning the layer hash.
      Uses day11's Build_layer.build with overlayfs stacking. *)
  let run (env : build_env) ~mounts ~dep_layers ~hash ~pkg =
    let node : Day11_opam_layer.Build.t =
      { hash; pkg; deps = [];
        universe = Day11_solution.Universe.dummy } in
    match Day11_opam_build.Build_layer.build env.env env.benv
            ~mounts ~build_dirs:dep_layers node () with
    | Day11_opam_build.Types.Success _bl ->
      Ok { pkg; hash }
    | _ ->
      Error (`Msg (Printf.sprintf "Build failed for %s"
        (OpamPackage.to_string pkg)))
end

(* ── Doc compile (produces .odoc files) ────────────────────────── *)

module Compile = struct
  type t = {
    pkg : OpamPackage.t;
    hash : string;
  }

  let pkg t = t.pkg
  let hash t = t.hash
  let layer_dir ~os_dir t = Day11_layer.Dir.path ~os_dir t.hash

  let is_cached ~os_dir t = layer_exists os_dir t.hash

  (** Compile docs for a package.
      Takes the build layer + dep compile layers, produces a compile layer. *)
  let run (env : doc_env) ~build_layer ~dep_compile_layers ~hash ~pkg =
    let config : Day11_doc.Doc_build.doc_config = {
      driver_tool = env.driver_tool;
      odoc_tool = env.odoc_tool;
      os_dir = env.build.os_dir;
      blessed = env.blessed;
    } in
    match Day11_doc.Doc_build.compile env.build.env env.build.benv
            ~config ~build_layer ~dep_compile_layers ~hash pkg with
    | Ok _layer_dir -> Ok { pkg; hash }
    | Error msg -> Error (`Msg msg)
end

(* ── Doc link (produces HTML) ──────────────────────────────────── *)

module Link = struct
  (** Link docs for a package.
      Takes compile layer + dep compile layers, writes HTML to html_dir. *)
  let run (env : doc_env) ~build_layer ~compile_layer
      ~dep_compile_layers ~html_dir ~hash ~pkg =
    let config : Day11_doc.Doc_build.doc_config = {
      driver_tool = env.driver_tool;
      odoc_tool = env.odoc_tool;
      os_dir = env.build.os_dir;
      blessed = env.blessed;
    } in
    Day11_doc.Doc_build.link env.build.env env.build.benv
      ~config ~build_layer ~compile_layer
      ~dep_compile_layers ~html_dir ~hash pkg
end

(* ── Doc all (compile + link in one step) ──────────────────────── *)

module Doc_all = struct
  type t = {
    pkg : OpamPackage.t;
    hash : string;
  }

  let pkg t = t.pkg
  let hash t = t.hash
  let layer_dir ~os_dir t = Day11_layer.Dir.path ~os_dir t.hash

  let is_cached ~os_dir t = layer_exists os_dir t.hash

  let run (env : doc_env) ~build_layer ~dep_compile_layers
      ~html_dir ~hash ~pkg =
    let config : Day11_doc.Doc_build.doc_config = {
      driver_tool = env.driver_tool;
      odoc_tool = env.odoc_tool;
      os_dir = env.build.os_dir;
      blessed = env.blessed;
    } in
    match Day11_doc.Doc_build.doc_all env.build.env env.build.benv
            ~config ~build_layer ~dep_compile_layers ~html_dir ~hash pkg with
    | Ok _layer_dir -> Ok { pkg; hash }
    | Error msg -> Error (`Msg msg)
end

(* ── Hash computation ──────────────────────────────────────────── *)

module Hash = struct
  (** Compute the build layer hash for a package from its solution. *)
  let build_hash ~base_hash ~find_opam pkgs =
    let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
    Day11_opam_build.Hash_cache.layer_hash cache ~base_hash pkgs

  (** Compute the base image hash, optionally using a pinned digest. *)
  let base_hash ~os_distribution ~os_version ~arch ?digest () =
    Day11_opam_build.Base.build_hash
      ~os_distribution ~os_version ~arch ?digest ()

  (** Compute the compile layer hash for a package. *)
  let compile_hash ~build_hash ~tool_hash ~blessed ~dep_compile_hashes =
    Day11_layer.Hash.of_strings
      ([ "compile"; build_hash; tool_hash;
         (if blessed then "blessed" else "unblessed") ]
       @ List.sort String.compare dep_compile_hashes)

  (** Compute the link layer hash. *)
  let link_hash ~compile_hash ~tool_hash ~blessed ~dep_compile_hashes =
    Day11_layer.Hash.of_strings
      ([ "link"; compile_hash; tool_hash;
         (if blessed then "blessed" else "unblessed") ]
       @ List.sort String.compare dep_compile_hashes)

  (** Compute the tool hash from driver + odoc tool. *)
  let tool_hash ~driver_tool ~odoc_tool =
    Day11_layer.Hash.of_strings
      [ (driver_tool : Day11_opam_layer.Tool.t).hash;
        (odoc_tool : Day11_opam_layer.Tool.t).hash ]

  (** Check if a build layer has documentable libraries. *)
  let has_documentable_libs layer_dir =
    Day11_doc.Doc_build.has_documentable_libs layer_dir
end
