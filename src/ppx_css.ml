open Migrate_parsetree
open Ast_406
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let rec expr mapper e =
  match e.pexp_desc with
  | Pexp_extension
      ({ txt; loc },
       PStr [{ pstr_desc = Pstr_eval (e, _) }]) when txt = "style" || txt = "css" ->
    begin match e.pexp_desc with
      | Pexp_constant Pconst_string (str, delim) ->
        let loc_start =
          match delim with
          | None -> e.pexp_loc.Location.loc_start
          | Some s ->
            { e.pexp_loc.Location.loc_start with
              Lexing.pos_cnum =
                e.pexp_loc.Location.loc_start.Lexing.pos_cnum + (String.length s) + 1
            }
        in
        let container_lnum = loc_start.Lexing.pos_lnum in
        let pos = loc_start in
        let mode =
          match delim with
          | None
          | Some ""
          | Some "bs-css" -> Css_to_ocaml.Bs_css
          | Some "typed"
          | Some "bs-typed-css" -> Css_to_ocaml.Bs_typed_css
          | _ ->
            raise
              (Location.Error
                 (Location.error ~loc
                    "Unexpected delimiter: accepted values are \"bs-css\" (or none) for bs-css, \
                     and \"typed\" (or \"bs-typed-css\") for bs-typed-css")) in
        begin match txt with
          | "style" ->
            let ast =
              Css_lexer.parse_string
                ~container_lnum
                ~pos
                str
                Css_parser.declaration_list in
            Css_to_ocaml.render_declaration_list mode ast
          | "css" -> 
            let ast =
              Css_lexer.parse_string
                ~container_lnum
                ~pos
                str
                Css_parser.stylesheet in
            Css_to_ocaml.render_stylesheet mode ast
          | _ -> assert false
        end
      | _ ->
        let message =
          match txt with
          | "css" -> "[%css] accepts a string, e.g. [%css \"{\n  color: red;\n}\"]"
          | "style" -> "[%style] accepts a string, e.g. [%style \"color: red;\"]"
          | _ -> assert false in
        raise
          (Location.Error
             (Location.error ~loc message))
    end
  | _ -> default_mapper.expr mapper e

let mapper _ _ = { default_mapper with expr }

let () = Driver.register ~name:"css" Versions.ocaml_406 mapper
