open Migrate_parsetree

let () =
  Driver.register
    ~name:"css"
    ~args:[]
    Versions.ocaml_410
    Ppx_css.mapper;

  let argv =
    match Sys.argv with
    | [| program; input_file; output_file |] ->
      [| program; input_file; "-o"; output_file; "--dump-ast" |]
    | _ ->
      Sys.argv
  in
  Migrate_parsetree.Driver.run_main ~argv ()

