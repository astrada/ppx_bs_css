open Migrate_parsetree
open Ast_406
open Ast_helper
open Asttypes
open Parsetree

let compose_loc loc ppx_loc =
  let compose_pos pos ppx_pos =
    (* It looks like positions (from menhir) get 2 extra chars in pos_cnum
       starting from the second line. *)
    let extra_chars = if pos.Lexing.pos_lnum > 1 then 2 else 0 in
    { Lexing.pos_fname = ppx_pos.Lexing.pos_fname;
      pos_lnum = ppx_pos.Lexing.pos_lnum +
                 pos.Lexing.pos_lnum - 1;
      pos_bol = ppx_pos.Lexing.pos_cnum +
                pos.Lexing.pos_bol;
      pos_cnum = ppx_pos.Lexing.pos_cnum +
                 pos.Lexing.pos_cnum - extra_chars;
    } in
  let loc_start = compose_pos loc.Location.loc_start ppx_loc.Location.loc_start in
  let loc_end = compose_pos loc.Location.loc_end ppx_loc.Location.loc_start in
  { Location.loc_start;
    loc_end;
    loc_ghost = loc.Location.loc_ghost;
  }

let rec render_component_value ((cv, cv_loc): Css_types.Component_value.t Css_types.with_loc) (ppx_loc: Location.t) : expression =
  let loc = compose_loc cv_loc ppx_loc in
  let render_block start_char end_char cs = assert false in
  match cv with
  | Css_types.Component_value.Brace_block cs -> render_block "{" "}" cs
  | Paren_block cs -> render_block "(" ")" cs
  | Bracket_block cs -> render_block "[" "]" cs
  | Percentage p -> assert false
  | Ident i ->
    Exp.ident ~loc { txt = Lident i; loc }
  | String s
  | Uri s
  | Operator s
  | Delim s
  | Hash s
  | Number s
  | Unicode_range s -> assert false
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

let render_declaration (d: Css_types.Declaration.t) (d_loc: Location.t) (ppx_loc: Location.t) : expression =
  let (name, name_loc) = d.Css_types.Declaration.name in
  let name_loc = compose_loc name_loc ppx_loc in
  let name = to_caml_case name in
  let (vs, _) = d.Css_types.Declaration.value in
  let parameter_count = List.length vs in
  let name =
    if parameter_count > 1 then
      name ^ (string_of_int parameter_count)
    else name in
  let ident = Exp.ident ~loc:name_loc { txt = Lident name; loc = name_loc } in
  let args =
    List.map (fun v -> render_component_value v ppx_loc) vs in
  Exp.apply ~loc:d_loc ident (List.map (fun a -> (Nolabel, a)) args)

let render_declaration_list ((dl, dl_loc): Css_types.Declaration_list.t) (ppx_loc: Location.t) : expression =
  let loc = compose_loc dl_loc ppx_loc in
  let end_loc = Lex_buffer.make_loc ~loc_ghost:true loc.Location.loc_end loc.Location.loc_end in
  List.fold_left
    (fun e d ->
       let (d_expr, d_loc) =
         match d with
         | Css_types.Declaration_list.Declaration decl ->
           let decl_loc = compose_loc decl.loc ppx_loc in
           render_declaration decl decl_loc ppx_loc, decl_loc
         | Css_types.Declaration_list.At_rule ar ->
           let ar_loc = compose_loc ar.loc ppx_loc in
           render_at_rule ar, ar_loc
       in
       let loc = Lex_buffer.make_loc ~loc_ghost:true d_loc.Location.loc_start loc.Location.loc_end in
       Exp.construct ~loc
         { txt = Lident "::"; loc }
         (Some (Exp.tuple ~loc [d_expr; e]));
    )
    (Exp.construct ~loc:end_loc
       { txt = Lident "[]"; loc = end_loc }
       None)
    (List.rev dl)

let render_style_rule (sr: Css_types.Style_rule.t) : expression = assert false

let render_rule (r: Css_types.Rule.t) : expression = assert false

let render_stylesheet (s: Css_types.Stylesheet.t) : expression = assert false
