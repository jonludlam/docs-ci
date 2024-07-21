type t = { config : Config.t; voodoo : Voodoo.t }

let version = "v1"
let v config voodoo = { config; voodoo }

type stage = [ `Linked | `Html ]

let digest stage t =
  let key =
    match stage with
    | `Html ->
        Fmt.str "%s:%s:%s:%s:%s" version (Config.odoc t.config)
          Voodoo.Do.(v t.voodoo |> digest)
          Voodoo.Prep.(v t.voodoo |> digest)
          Voodoo.OdocDriver.(v ~odoc_pin:(Config.odoc t.config) ~sherlodoc_pin:(Config.sherlodoc t.config) |> digest)
    | `Linked ->
        Fmt.str "%s:%s:%s:%s" version (Config.odoc t.config)
          Voodoo.Do.(v t.voodoo |> digest)
          Voodoo.Prep.(v t.voodoo |> digest)
  in
  key |> Digest.string |> Digest.to_hex

let pp f t =
  Fmt.pf f
    "docs-ci: %s\nodoc: %s\nvoodoo do: %a\nvoodoo prep: %a\nvoodoo gen: %s"
    version (Config.odoc t.config) Current_git.Commit_id.pp
    Voodoo.Do.(v t.voodoo |> commit)
    Current_git.Commit_id.pp
    Voodoo.Prep.(v t.voodoo |> commit)
    Voodoo.OdocDriver.(v ~odoc_pin:(Config.odoc t.config) ~sherlodoc_pin:(Config.sherlodoc t.config) |> pin)
