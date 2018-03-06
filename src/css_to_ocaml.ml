open Migrate_parsetree
open Ast_406
open Ast_helper
open Asttypes
open Parsetree
open Css_types

type mode =
  | Bs_css
  | Bs_typed_css

let grammar_error loc message =
  raise (Css_lexer.GrammarError (message, loc))

let is_overloaded mode declaration =
  (* Overloaded declarations are rendered as function applications where function name
     ends with a number that specifies the number of parameters.
     E.g.: margin: 1em 2px -> margin2 em(1.) px(2.)
  *)
  match mode with
  | Bs_css ->
    begin match declaration with
      | "margin"
      | "padding" -> true
      | _ -> false
    end
  | Bs_typed_css -> true

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

let string_to_const ~loc s =
  Exp.constant ~loc (Const.string ~quotation_delimiter:"js" s)

let rec render_component_value mode ((cv, loc): Component_value.t with_loc) : expression =
  let render_block start_char end_char cs =
    grammar_error loc ("Unsupported " ^ start_char ^ "-block")
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

  let render_function (name, name_loc) (params, params_loc) =
    let caml_case_name = to_caml_case name in
    let ident =
      Exp.ident ~loc:name_loc { txt = Lident caml_case_name; loc = name_loc } in
    let grouped_params =
      let rec group_param (accu, loc) xs =
        match xs with
        | [] -> (accu, loc), []
        | (Component_value.Delim ",", _) :: rest -> (accu, loc), rest
        | (cv, cv_loc) as hd :: rest ->
          let loc =
            let loc_start =
              if loc = Location.none then cv_loc.Location.loc_start
              else loc.Location.loc_start in
            Lex_buffer.make_loc loc_start cv_loc.Location.loc_end in
          group_param (accu @ [hd], loc) rest
      in
      let rec group_params accu xs =
        match xs with
        | [] -> accu
        | _ ->
          let param, rest = group_param ([], Location.none) xs in
          group_params (accu @ [param]) rest
      in
      group_params [] params
    in
    let args =
      let open Component_value in
      let side_or_corner_expr deg loc =
        render_component_value mode
          (Float_dimension (deg, "deg"), loc)
      in
      match name with
      | "linear-gradient"
      | "repeating-linear-gradient" ->
        let (side_or_corner, color_stop_params) =
          match List.hd grouped_params with
          | ([(Float_dimension (_, "deg"), _) as cv], _) ->
            render_component_value mode cv, List.tl grouped_params
          | ([(Ident "to", _);
              (Ident "bottom", _)], loc) ->
            side_or_corner_expr "180" loc, List.tl grouped_params
          | ([(Ident "to", _);
              (Ident "top", _)], loc) ->
            side_or_corner_expr "0" loc, List.tl grouped_params
          | ([(Ident "to", _);
              (Ident "right", _)], loc) ->
            side_or_corner_expr "90" loc, List.tl grouped_params
          | ([(Ident "to", _);
              (Ident "left", _)], loc) ->
            side_or_corner_expr "270" loc, List.tl grouped_params
          | ((Ident _, _) :: _, _) ->
            let implicit_side_or_corner_loc =
              Lex_buffer.make_loc ~loc_ghost:true
                params_loc.Location.loc_start
                params_loc.Location.loc_start in
            render_component_value mode
              (Float_dimension ("180", "deg"), implicit_side_or_corner_loc), grouped_params
          | (_, loc) ->
            grammar_error loc "Unexpected first parameter"
          | exception (Failure _) ->
            grammar_error params_loc "Missing parameters"
        in
        let color_stops =
          List.rev_map
            (function
              | ([(color, start_loc) as color_cv;
                  (Percentage perc, end_loc)], _) ->
                let color_expr = render_component_value mode color_cv in
                let perc_expr = Exp.constant ~loc:end_loc (number_to_const perc) in
                let loc =
                  Lex_buffer.make_loc start_loc.Location.loc_start end_loc.Location.loc_end in
                Exp.tuple ~loc [perc_expr; color_expr]
              | (_, loc) ->
                grammar_error loc "Unexpected color stop"
            )
            color_stop_params
        in
        let end_loc =
          Lex_buffer.make_loc ~loc_ghost:true loc.Location.loc_end loc.Location.loc_end in
        let color_stop_expr =
          List.fold_left
            (fun e param ->
               Exp.construct ~loc
                 { txt = Lident "::"; loc }
                 (Some (Exp.tuple ~loc [param; e]))
            )
            (Exp.construct ~loc:end_loc
               { txt = Lident "[]"; loc = end_loc }
               None)
            color_stops in
        [side_or_corner; color_stop_expr]
      | _ ->
        params
        |> List.filter
          (function (Delim ",", _) -> false | _ -> true)
        |> List.map
          (function
            | (Number "0", loc) ->
              Exp.constant ~loc (Const.int 0)
            | c -> render_component_value mode c)
    in
    Exp.apply ~loc ident (List.map (fun a -> (Nolabel, a)) args)
  in

  match cv with
  | Component_value.Paren_block cs -> render_block "(" ")" cs
  | Bracket_block cs -> render_block "[" "]" cs
  | Percentage p ->
    let ident = Exp.ident ~loc { txt = Lident "pct"; loc } in
    let const = float_to_const p in
    let arg = Exp.constant ~loc const in
    Exp.apply ~loc ident [(Nolabel, arg)]
  | Ident i ->
    let name = to_caml_case i in
    Exp.ident ~loc { txt = Lident name; loc }
  | String s ->
    string_to_const ~loc s
  | Uri s ->
    let ident = Exp.ident ~loc { txt = Lident "url"; loc } in
    let arg = string_to_const ~loc s in
    Exp.apply ~loc ident [(Nolabel, arg)]
  | Operator s ->
    grammar_error loc "Unsupported operator"
  | Delim s ->
    grammar_error loc "Unsupported delimiter"
  | Hash s ->
    let ident = Exp.ident ~loc { txt = Lident "hex"; loc } in
    let arg = string_to_const ~loc s in
    Exp.apply ~loc ident [(Nolabel, arg)]
  | Number s ->
    if s = "0" then Exp.ident ~loc { txt = Lident "zero"; loc }
    else Exp.constant ~loc (number_to_const s)
  | Unicode_range s ->
    grammar_error loc "Unsupported unicode range"
  | Function (f, params) ->
    render_function f params
  | Float_dimension (number, dimension) ->
    let const =
      if mode = Bs_css && dimension = "deg" then
        (* bs-css uses int degrees *)
        Const.integer number
      else
        float_to_const number in
    render_dimension number dimension const
  | Dimension (number, dimension) ->
    let const = number_to_const number in
    render_dimension number dimension const

and render_at_rule mode (ar: At_rule.t) : expression =
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
                     | (_, loc) ->
                       grammar_error loc "Unexpected @keyframes prelude"
                   end in
                 let block_expr =
                   render_declaration_list mode sr.Style_rule.block in
                 let tuple =
                   Exp.tuple ~loc:sr.Style_rule.loc [progress_expr; block_expr] in
                 let loc =
                   Lex_buffer.make_loc
                     ~loc_ghost:true sr.Style_rule.loc.Location.loc_start loc.Location.loc_end in
                 Exp.construct ~loc
                   { txt = Lident "::"; loc }
                   (Some (Exp.tuple ~loc [tuple; e]));
               | Rule.At_rule ar ->
                 grammar_error ar.At_rule.loc "Unexpected at-rule in @keyframes body"
            )
            (Exp.construct ~loc:end_loc
               { txt = Lident "[]"; loc = end_loc }
               None)
            (List.rev rs) in
        Exp.apply ~loc:ar.At_rule.loc ident [(Nolabel, arg)]
      | _ -> assert false
    end
  | (n, _) ->
    grammar_error ar.At_rule.loc ("At-rule @" ^ n ^ " not supported")

and render_declaration mode (d: Declaration.t) (d_loc: Location.t) : expression =
  let (name, name_loc) = d.Declaration.name in
  let name = to_caml_case name in
  let (vs, _) = d.Declaration.value in
  let name =
    if is_overloaded mode name then
      let parameter_count = List.length vs in
      if parameter_count > 1 then
        name ^ (string_of_int parameter_count)
      else name
    else name in
  let ident = Exp.ident ~loc:name_loc { txt = Lident name; loc = name_loc } in
  let args =
    List.map (fun v -> render_component_value mode v) vs in
  Exp.apply ~loc:d_loc ident (List.map (fun a -> (Nolabel, a)) args)

and render_declaration_list mode ((dl, loc): Declaration_list.t) : expression =
  let end_loc =
    Lex_buffer.make_loc ~loc_ghost:true loc.Location.loc_end loc.Location.loc_end in
  List.fold_left
    (fun e d ->
       let (d_expr, d_loc) =
         match d with
         | Declaration_list.Declaration decl ->
           render_declaration mode decl decl.loc, decl.loc
         | Declaration_list.At_rule ar ->
           render_at_rule mode ar, ar.loc
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

and render_style_rule mode (sr: Style_rule.t) : expression =
  let (prelude, prelude_loc) = sr.Style_rule.prelude in
  let selector =
    List.fold_left
      (fun s (value, value_loc) ->
         match value with
         | Component_value.Delim ":" -> ":" ^ s
         | Ident v
         | Operator v
         | Delim v -> if String.length s > 0 then v ^ " " ^ s else v ^ s
         | _ ->
           grammar_error value_loc "Unexpected selector"
      )
      ""
      (List.rev prelude) in
  let selector_expr = string_to_const ~loc:prelude_loc selector in
  let dl_expr = render_declaration_list mode sr.Style_rule.block in
  let ident = Exp.ident ~loc:prelude_loc { txt = Lident "selector"; loc = prelude_loc } in
  Exp.apply ~loc:sr.Style_rule.loc ident [(Nolabel, selector_expr); (Nolabel, dl_expr)]

and render_rule mode (r: Rule.t) : expression =
  match r with
  | Rule.Style_rule sr -> render_style_rule mode sr
  | Rule.At_rule ar -> render_at_rule mode ar

and render_stylesheet mode ((rs, loc): Stylesheet.t) : expression =
  let end_loc =
    Lex_buffer.make_loc ~loc_ghost:true loc.Location.loc_end loc.Location.loc_end in
  List.fold_left
    (fun e r ->
       let (r_expr, r_loc) =
         match r with
         | Rule.Style_rule sr -> render_rule mode r, sr.Style_rule.loc
         | Rule.At_rule ar -> render_rule mode r, ar.At_rule.loc in
       let loc =
         Lex_buffer.make_loc
           ~loc_ghost:true r_loc.Location.loc_start loc.Location.loc_end in
       Exp.construct ~loc
         { txt = Lident "::"; loc }
         (Some (Exp.tuple ~loc [r_expr; e]));
    )
    (Exp.construct ~loc:end_loc
       { txt = Lident "[]"; loc = end_loc }
       None)
    (List.rev rs)
