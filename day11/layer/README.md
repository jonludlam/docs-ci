# layer — Generic on-disk layer cache primitives

A domain-agnostic abstraction for content-addressed build layers stored
on disk. Knows about layer directories, generic JSON metadata, file
scanning, hardlink merging, and overlay-mount planning. Knows nothing
about opam, packages, doc generation, or any specific build pipeline.

For opam-flavoured layers see the sister library
[`day11_opam_layer`](../opam_layer/), which adds the
package-specific sidecars and helpers.

## External dependencies

- `day11_sys` — sudo wrappers, subprocess execution
- `bos`, `fpath`, `rresult` — filesystem helpers
- `yojson` + `ppx_deriving_yojson` — `layer.json` serialization

Does NOT depend on `opam-format`, the solver, the doc pipeline, or
the container orchestration.

## On-disk layout

Every layer is a self-contained directory under `{os_dir}/`:

```
build-{hash[:12]}/
  layer.json      — generic metadata (see Layer_meta.t)
  layer.log       — captured stdout/stderr from the build
  last_used       — sentinel file; mtime is the LRU timestamp
  fs/             — overlay upper: new/changed files from the build

  build.json      — opam-package-build sidecar (Day11_opam_layer.Build_meta)
  doc.json        — odoc layer sidecar         (Day11_opam_layer.Doc_meta)
  ...             — future domains can add their own sidecars

packages/{id}/
  build-{hash}    — symlink registry, see Layer_symlinks
```

The "kind" of a layer is determined by which sidecar files exist in
its directory, not by a field inside `layer.json`. The generic library
never enumerates the set of possible kinds; each domain library owns
its own sidecar format and the convention for which filename it uses.

**Sidecar files must be valid JSON** (typically a single object at
the top level). This convention is unenforced by the library — no
part of `day11_layer` reads sidecars — but tools like
`day11-layer-cli` rely on it so they can pretty-print sidecar
contents without depending on any domain-specific library.

## Modules

### `Layer_type`

```ocaml
type base = { hash : string; dir : Fpath.t; image : string }
```

The base image layer at the bottom of every overlay stack. Recursive
build-DAG types live in domain libraries (e.g.
`Day11_opam_layer.Build`).

### `Hash`

Content-addressed hashing primitives.

```ocaml
val of_strings : string list -> string
val base_hash : image:string -> string
val layer_hash :
  base_hash:string -> dep_hashes:string list -> pkg:string -> string
```

### `Layer_dir`

```ocaml
val name : string -> string
val path : os_dir:Fpath.t -> string -> Fpath.t
```

The `build-XXXXXXXXXXXX` directory naming convention. The `build-`
prefix is historical; this module uses it uniformly across all layer
kinds.

### `Layer_meta`

The generic per-layer JSON record stored as `layer.json`.

```ocaml
type timing = (string * float) list

type t = {
  exit_status : int;
  parent_hashes : string list;
  uid : int;
  gid : int;
  base_hash : string;
  disk_usage : int;
  timing : timing;
  created_at : string;
  failed_dep : string option;
}

val save : ?created_at:string -> Fpath.t -> t -> (unit, _) result
val load : Fpath.t -> (t, _) result
```

Domain-specific information lives in sidecar JSON files in the same
directory, owned by the relevant domain library.

### `Last_used`

```ocaml
val touch : Fpath.t -> unit
val get : Fpath.t -> float option
```

Per-layer LRU sentinel. Cheap enough to call from every dep-lookup
path. Split off from `Layer_meta` so a touch doesn't rewrite JSON.

### `Stack`

Layer combining for overlayfs assembly.

```ocaml
val merge : env -> layer_dirs:Fpath.t list -> target:Fpath.t -> (unit, _) result
val plan_lowerdir :
  available:int ->
  merged_overhead:int ->
  entry_cost:(Fpath.t -> int) ->
  Fpath.t list ->
  Fpath.t list * Fpath.t list
```

`merge` cp-hardlinks the `fs/` of every layer into a single target
directory. `plan_lowerdir` decides which layers to keep as separate
overlayfs lowerdirs and which to merge, given a byte budget for the
mount-options string.

### `Layer_symlinks`

```ocaml
val ensure :
  packages_dir:Fpath.t -> id:string -> layer_name:string ->
  (unit, _) result
```

A small per-id registry. The `id` is opaque to this library — opam
callers pass `name.version` strings, but anything goes.

### `Scan`

Directory enumeration: list layer dirs and per-id symlinks. Generic;
caller filters and interprets.

### `Import`

Bootstrap a base layer by exporting a Docker image.

## Error-handling convention

| Operation kind | Returns |
|---|---|
| Read, maybe-exists lookup | `_ option` |
| Read, must-exist or parse | `(_, [> Rresult.R.msg]) result` |
| Write / create / mutate | `(_, [> Rresult.R.msg]) result` |
| Best-effort bookkeeping (LRU touch) | `unit`, silent |

## Testing

Unit tests in `test/` cover every module without requiring root or a
container runtime. They run on any platform where OCaml builds.
Integration tests that actually mount overlayfs live in
`day11/container/test/` and are gated on `DAY11_INTEGRATION=true`.
