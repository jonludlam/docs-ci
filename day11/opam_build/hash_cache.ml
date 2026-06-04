type t = {
  find_opam : OpamPackage.t -> OpamFile.OPAM.t option;
  patches : Patches.t option;
  per_pkg : (string, string) Hashtbl.t;
  per_layer : (string, string) Hashtbl.t;
}

let create ~find_opam ?patches () =
  { find_opam; patches;
    per_pkg = Hashtbl.create 256;
    per_layer = Hashtbl.create 256; }

let pkg_opam_hash t pkg =
  let key = OpamPackage.to_string pkg in
  match Hashtbl.find_opt t.per_pkg key with
  | Some h -> h
  | None ->
      let opam_h =
        match t.find_opam pkg with
        | Some opam ->
            opam
            |> OpamFile.OPAM.effective_part
            |> OpamFile.OPAM.write_to_string
            |> Digest.string |> Digest.to_hex
        | None -> "missing-" ^ key
      in
      let h = match t.patches with
        | Some patches ->
          let ph = Patches.hash_for patches pkg in
          if ph = "" then opam_h
          else Digest.string (opam_h ^ ph) |> Digest.to_hex
        | None -> opam_h
      in
      Hashtbl.replace t.per_pkg key h;
      h

let layer_hash t ~base_hash pkgs =
  (* Digest the (base, pkg_list) concatenation so the Hashtbl key is
     a fixed-size 32-byte digest rather than a 1-10KB string. With
     universes of 50-100 packages and 4000+ solutions, an un-digested
     key made per-entry Hashtbl ops O(universe_size) and dominated
     [build_dag] wall-clock. *)
  let str = String.concat ","
    (base_hash :: List.map OpamPackage.to_string pkgs) in
  let key = Digest.to_hex (Digest.string str) in
  match Hashtbl.find_opt t.per_layer key with
  | Some h -> h
  | None ->
      let hashes =
        List.map (fun pkg -> pkg_opam_hash t pkg) pkgs
      in
      let h = Day11_layer.Hash.of_strings (base_hash :: hashes) in
      Hashtbl.replace t.per_layer key h;
      h
