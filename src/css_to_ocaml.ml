open Migrate_parsetree
open Ast_406
open Ast_helper
open Asttypes
open Parsetree

let rec render_component_value (cv: Css_types.Component_value.t) : expression =
  let render_block start_char end_char cs = assert false in
  match cv with
  | Css_types.Component_value.Brace_block cs -> render_block "{" "}" cs
  | Paren_block cs -> render_block "(" ")" cs
  | Bracket_block cs -> render_block "[" "]" cs
  | Percentage p -> assert false
  | Ident s ->
    Exp.ident {txt=Lident s;loc=Location.none}
  | String s
  | Uri s
  | Operator s
  | Delim s
  | Hash s
  | Number s
  | Unicode_range s ->
    assert false
  | At_rule ar -> render_at_rule ar
  | Function (name, params) -> assert false
  | Dimension (number, dimension) -> assert false


and render_at_rule (ar: Css_types.At_rule.t) : expression = assert false

let split c s =
  let rec loop s accu =
    try
      let index = String.index s c in
      (String.sub s 0 index) :: (loop (String.sub s (index + 1) (String.length s - index - 1)) accu)
    with Not_found -> s :: accu
  in
  loop s []

let to_caml_case s =
  let splitted = split '-' s in
  List.fold_left
    (fun s part ->
       s ^ (if s <> "" then String.capitalize part else part))
    ""
    splitted

let render_declaration (d: Css_types.Declaration.t) : expression =
  let name = to_caml_case d.Css_types.Declaration.name in
  let parameter_count = List.length d.Css_types.Declaration.value in
  let name =
    if parameter_count > 1 then
      name ^ (string_of_int parameter_count)
    else name in
  let ident = Exp.ident ({txt=Lident name; loc = Location.none}) in
  let args = List.map render_component_value d.Css_types.Declaration.value in
  Exp.apply ident (List.map (fun a -> (Nolabel, a)) args)

let render_declaration_list (dl: Css_types.Declaration_list.t) : expression =
  List.fold_left
    (fun e d ->
       let d_expr =
         match d with
         | Css_types.Declaration_list.Declaration decl -> render_declaration decl
         | Css_types.Declaration_list.At_rule ar -> render_at_rule ar
       in
       Exp.construct
         ({txt = Lident "::"; loc = Location.none})
         (Some (Exp.tuple [d_expr; e]));
    )
    (Exp.construct
       ({txt = Lident "[]"; loc = Location.none})
       None)
    (List.rev dl)

let render_style_rule (sr: Css_types.Style_rule.t) : expression = assert false

let render_rule (r: Css_types.Rule.t) : expression = assert false

let render_stylesheet (s: Css_types.Stylesheet.t) : expression = assert false
