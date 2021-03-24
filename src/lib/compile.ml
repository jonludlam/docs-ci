type t = { package : Package.t; blessed : bool; odoc : Mld.Gen.odoc_dyn }

let is_blessed t = t.blessed

let odoc t = t.odoc

let package t = t.package

let network = Voodoo.network

let folder ~blessed package =
  let universe = Package.universe package |> Package.Universe.hash in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  if blessed then Fpath.(v "compile" / "packages" / name / version)
  else Fpath.(v "compile" / "universes" / universe / name / version)

let import_deps t =
  let folders = List.map (fun { package; blessed; _ } -> folder ~blessed package) t in
  Misc.rsync_pull folders

let spec ~base ~deps ~blessed prep =
  let open Obuilder_spec in
  let prep_folder = Prep.folder prep in
  let package = Prep.package prep in
  let compile_folder = folder ~blessed package in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in

  let odoc_package =
    Mld.{ file = Fpath.(compile_folder / "the_page.mld"); target = None; name = "the_page"; kind = Mld }
  in
  (* let package = Prep.package target in
     let package_name = OpamPackage.name (Package.opam package) |> OpamPackage.Name.to_string in
      let is_blessed = Package.Blessed.is_blessed blessed package in*)
  ( Voodoo.spec ~base ~prep:true ~link:true
    |> Spec.add
         [
           run ~network "opam pin -ny odoc %s && opam depext -iy odoc" Config.odoc;
           workdir "/home/opam/docs/";
           run "sudo chown opam:opam .";
           import_deps deps;
           run "echo hiya    ";
           Misc.rsync_pull [ prep_folder ];
           run "find . -type d";
           run "%s" @@ Fmt.str "mkdir -p %a" Fpath.pp compile_folder;
           (* workdir "/home/opam/docs/compile"; *)
           run "rm -f compile/packages.mld compile/page-packages.odoc compile/packages/*.mld compile/packages/*.odoc";
           run "rm -f compile/packages/%s/*.odoc" name;
           run "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-do -p %s %s" name (if blessed then "-b" else "");
           run "mkdir -p html";
           run ~secrets:Config.ssh_secrets ~network "rsync -avzR /home/opam/docs/./compile/ %s:%s/"
             Config.ssh_host Config.storage_folder;
           run ~secrets:Config.ssh_secrets ~network "rsync -avzR /home/opam/docs/./html/ %s:%s/"
             Config.ssh_host Config.storage_folder;
         ],
    odoc_package )

let folder { package; blessed; _ } = folder ~blessed package

let v ~blessed ~deps target =
  let open Current.Syntax in
  let spec =
    let+ deps = deps and+ prep = target and+ blessed = blessed in
    let package = Prep.package prep in
    let blessed = Package.Blessed.is_blessed blessed package in
    let spec, odoc = spec ~base:(Misc.get_base_image (Prep.package prep)) ~deps ~blessed prep in
    (spec |> Spec.to_ocluster_spec, odoc)
  in
  let odoc = Current.map snd spec in
  let spec = Current.map fst spec in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v ~secrets:Config.ssh_secrets_values conn in
  let+ () =
    let* target = target in
    Current_ocluster.build_obuilder
      ~label:(Fmt.str "odoc\n%s" (Prep.package target |> Package.opam |> OpamPackage.to_string))
      ~src:(Current.return []) ~pool:Config.pool ~cache_hint:"docs-universe-build" cluster spec
  and+ odoc = odoc
  and+ prep = target
  and+ blessed = blessed in
  let package = Prep.package prep in
  let blessed = Package.Blessed.is_blessed blessed package in
  { package; blessed; odoc = Mld odoc }
