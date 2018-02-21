open Migrate_parsetree
open Ast_406
open Ast_helper
open Asttypes
open Parsetree
open Css_types

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

let number_to_const number =
  if String.contains number '.' then Const.float number
  else Const.integer number

let float_to_const number =
  let number =
    if String.contains number '.' then number
    else number ^ "." in
  Const.float number

let rec render_component_value ((cv, cv_loc): Component_value.t with_loc) : expression =
  let loc = Css_lexer.fix_loc cv_loc in
  let string_to_const s =
    Exp.constant ~loc (Const.string ~quotation_delimiter:"js" s)
  in
  let render_dimension number dimension const =
    let number_loc =
      { loc with
        Location.loc_end =
          { loc.Location.loc_end with
            Lexing.pos_cnum =
              loc.Location.loc_end.Lexing.pos_cnum - (String.length dimension)
          };
      } in
    let dimension_loc =
      { loc with
        Location.loc_start =
          { loc.Location.loc_start with
            Lexing.pos_cnum =
              loc.Location.loc_start.Lexing.pos_cnum + (String.length number)
          };
      } in
    let ident =
      Exp.ident ~loc:dimension_loc { txt = Lident dimension; loc = dimension_loc } in
    let arg =
      Exp.constant ~loc:number_loc const in
    Exp.apply ~loc ident [(Nolabel, arg)]
  in  
  let render_block start_char end_char cs = assert false in
  match cv with
  | Component_value.Paren_block cs -> render_block "(" ")" cs
  | Bracket_block cs -> render_block "[" "]" cs
  | Percentage p ->
    let ident = Exp.ident ~loc { txt = Lident "pct"; loc } in
    let const = float_to_const p in
    let arg = Exp.constant ~loc const in
    Exp.apply ~loc ident [(Nolabel, arg)]
  | Ident i ->
    Exp.ident ~loc { txt = Lident i; loc }
  | String s ->
    string_to_const s
  | Uri s ->
    let ident = Exp.ident ~loc { txt = Lident "url"; loc } in
    let arg = string_to_const s in
    Exp.apply ~loc ident [(Nolabel, arg)]
  | Operator s
  | Delim s -> assert false
  | Hash s ->
    let ident = Exp.ident ~loc { txt = Lident "hex"; loc } in
    let arg = string_to_const s in
    Exp.apply ~loc ident [(Nolabel, arg)]
  | Number s ->
    if s = "0" then Exp.ident ~loc { txt = Lident "zero"; loc }
    else Exp.constant ~loc (number_to_const s)
  | Unicode_range s -> assert false
  | Function ((name, name_loc), params) ->
    let ident = Exp.ident ~loc:name_loc { txt = Lident name; loc = name_loc } in
    let args =
      params
      |> List.filter
        (function (Component_value.Delim ",", _) -> false | _ -> true)
      |> List.map
        (function
          | (Component_value.Number "0", loc) ->
            Exp.constant ~loc (Const.int 0)
          | c -> render_component_value c) in
    Exp.apply ~loc ident (List.map (fun a -> (Nolabel, a)) args)
  | Float_dimension (number, dimension) ->
    let const = float_to_const number in
    render_dimension number dimension const
  | Dimension (number, dimension) ->
    let const = number_to_const number in
    render_dimension number dimension const
and render_at_rule (ar: At_rule.t) : expression =
  match ar.At_rule.name with
  | ("keyframes" as n, loc) ->
    let ident = Exp.ident ~loc { txt = Lident n; loc } in
    begin match ar.At_rule.block with
      | Brace_block.Stylesheet (rs, loc) ->
        let end_loc =
          Lex_buffer.make_loc ~loc_ghost:true loc.Location.loc_end loc.Location.loc_end in
        let arg =
          List.fold_left
            (fun e r ->
               match r with
               | Rule.Style_rule sr ->
                 let progress_expr =
                   begin match sr.Style_rule.prelude with
                     | ([Component_value.Percentage p, loc], _) ->
                       Exp.constant ~loc (number_to_const p)
                     | ([Component_value.Ident "from", loc], _) ->
                       Exp.constant ~loc (Const.int 0)
                     | ([Component_value.Ident "to", loc], _) ->
                       Exp.constant ~loc (Const.int 100)
                     | _ -> failwith "Unexpected @keyframes prelude"
                   end in
                 let block_expr =
                   render_declaration_list sr.Style_rule.block in
                 let tuple =
                   Exp.tuple ~loc:sr.Style_rule.loc [progress_expr; block_expr] in
                 let loc =
                   Lex_buffer.make_loc
                     ~loc_ghost:true sr.Style_rule.loc.Location.loc_start loc.Location.loc_end in
                 Exp.construct ~loc
                   { txt = Lident "::"; loc }
                   (Some (Exp.tuple ~loc [tuple; e]));
               | _ -> failwith "Unexpected at-rule in @keyframes body"
            )
            (Exp.construct ~loc:end_loc
               { txt = Lident "[]"; loc = end_loc }
               None)
            (List.rev rs) in
        Exp.apply ~loc:ar.At_rule.loc ident [(Nolabel, arg)]
      | _ -> failwith "Unexpected @keyframes body"
    end
  | (n, _) -> failwith ("At-rule @" ^ n ^ " not supported")

and render_declaration (d: Declaration.t) (d_loc: Location.t) : expression =
  let (name, name_loc) = d.Declaration.name in
  let name_loc = Css_lexer.fix_loc name_loc in
  let name = to_caml_case name in
  let (vs, _) = d.Declaration.value in
  let parameter_count = List.length vs in
  let name =
    if parameter_count > 1 then
      name ^ (string_of_int parameter_count)
    else name in
  let ident = Exp.ident ~loc:name_loc { txt = Lident name; loc = name_loc } in
  let args =
    List.map (fun v -> render_component_value v) vs in
  Exp.apply ~loc:d_loc ident (List.map (fun a -> (Nolabel, a)) args)

and render_declaration_list ((dl, dl_loc): Declaration_list.t) : expression =
  let loc = Css_lexer.fix_loc dl_loc in
  let end_loc = Lex_buffer.make_loc ~loc_ghost:true loc.Location.loc_end loc.Location.loc_end in
  List.fold_left
    (fun e d ->
       let (d_expr, d_loc) =
         match d with
         | Declaration_list.Declaration decl ->
           let decl_loc = Css_lexer.fix_loc decl.loc in
           render_declaration decl decl_loc, decl_loc
         | Declaration_list.At_rule ar ->
           let ar_loc = Css_lexer.fix_loc ar.loc in
           render_at_rule ar, ar_loc
       in
       let loc =
         Lex_buffer.make_loc
           ~loc_ghost:true d_loc.Location.loc_start loc.Location.loc_end in
       Exp.construct ~loc
         { txt = Lident "::"; loc }
         (Some (Exp.tuple ~loc [d_expr; e]));
    )
    (Exp.construct ~loc:end_loc
       { txt = Lident "[]"; loc = end_loc }
       None)
    (List.rev dl)

and render_style_rule (sr: Style_rule.t) : expression = assert false

and render_rule (r: Rule.t) : expression = assert false

and render_stylesheet (s: Stylesheet.t) : expression = assert false
