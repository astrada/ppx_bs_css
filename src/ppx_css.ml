open Migrate_parsetree
open Ast_406
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let rec expr mapper e =
  match e.pexp_desc with
  | Pexp_extension ({ txt = "css"; loc}, PStr [{ pstr_desc = Pstr_eval (e, _) }])
    -> (match e.pexp_desc with
        | Pexp_constant Pconst_string (str, delim) ->
          let ast = Css_lexer.parse_string str Css_parser.declaration_list in
          Css_to_ocaml.render_declaration_list ast e.pexp_loc
        | _ ->
          raise
            (Location.Error
               (Location.error ~loc
                  "[%css] accepts a string, e.g. [%css \"{\n  color: red;\n}\"]"))
      )
  | _ -> default_mapper.expr mapper e


let mapper _ _ = { default_mapper with expr }

let () = Driver.register ~name:"css" Versions.ocaml_406 mapper
