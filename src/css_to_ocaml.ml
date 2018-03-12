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

let is_variant mode ident =
  (* bs-css uses polymorphic variants to enumerate values. Some of them
     have corresponding constant, but some others have conflicting names
     (e.g.: left) or don't have a corresponding constant (e.g.: justify).
  *)
  match mode with
  | Bs_css ->
    begin match ident with
      (* float/clear/text-align *)
      | "left"
      | "right"
      | "justify"
      (* cursor *)
      | "pointer"
      | "alias"
      | "all-scroll"
      | "cell"
      | "context-menu"
      | "default"
      | "crosshair"
      | "copy"
      | "grab"
      | "grabbing"
      | "help"
      | "move"
      | "not-allowed"
      | "progress"
      | "text"
      | "wait"
      | "zoom-in"
      | "zoom-out"
      (* list-style-type *)
      | "disc"
      | "circle"
      | "decimal"
      | "lower-alpha"
      | "upper-alpha"
      | "lower-greek"
      | "upper-greek"
      | "lower-latin"
      | "upper-latin"
      | "lower-roman"
      | "upper-roman"
      (* outline-style *)
      | "groove"
      | "ridge"
      | "inset"
      | "outset"
      (* transform-style *)
      | "preserve-3d"
      | "flat"
      (* font-variant *)
      | "small-caps" -> true
      | _ -> false
    end
  | Bs_typed_css -> false

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

let list_to_expr end_loc xs =
  List.fold_left
    (fun e param ->
       let loc =
         Lex_buffer.make_loc
           ~loc_ghost:true e.pexp_loc.Location.loc_start end_loc.Location.loc_end in
       Exp.construct ~loc
         { txt = Lident "::"; loc }
         (Some (Exp.tuple ~loc [param; e]))
    )
    (Exp.construct ~loc:end_loc
       { txt = Lident "[]"; loc = end_loc }
       None)
    xs

let group_params params =
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
    let grouped_params = group_params params in
    let rcv = render_component_value mode in
    let args =
      let open Component_value in
      let side_or_corner_expr deg loc =
        rcv (Float_dimension (deg, "deg"), loc)
      in
      let color_stops_to_expr_list color_stop_params =
        List.rev_map
          (function
            | ([(color, start_loc) as color_cv;
                (Percentage perc, end_loc)], _)
            | ([(color, start_loc) as color_cv;
                (Number ("0" as perc), end_loc)], _) ->
              let color_expr = rcv color_cv in
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
      match name with
      | "linear-gradient"
      | "repeating-linear-gradient" ->
        let (side_or_corner, color_stop_params) =
          match List.hd grouped_params with
          | ([(Float_dimension (_, "deg"), _) as cv], _) ->
            rcv cv, List.tl grouped_params
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
            rcv
              (Float_dimension ("180", "deg"), implicit_side_or_corner_loc), grouped_params
          | (_, loc) ->
            grammar_error loc "Unexpected first parameter"
          | exception (Failure _) ->
            grammar_error params_loc "Missing parameters"
        in
        let color_stops = color_stops_to_expr_list color_stop_params in
        let color_stop_expr = list_to_expr end_loc color_stops in
        [side_or_corner; color_stop_expr]
      | "radial-gradient"
      | "repeating-radial-gradient" ->
        let color_stops = color_stops_to_expr_list grouped_params in
        let color_stop_expr = list_to_expr end_loc color_stops in
        [color_stop_expr]
      | _ ->
        params
        |> List.filter
          (function (Delim ",", _) -> false | _ -> true)
        |> List.map
          (function
            | (Number "0", loc) ->
              Exp.constant ~loc (Const.int 0)
            | c -> rcv c)
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
    if is_variant mode i then
      Exp.variant ~loc name None
    else  
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
      if mode = Bs_css && (dimension = "deg" || dimension = "pt") then
        (* bs-css uses int degrees and points *)
        Const.integer number
      else
        float_to_const number in
    render_dimension number dimension const
  | Dimension (number, "ms") when mode = Bs_css ->
    let const = Const.integer number in
    Exp.constant ~loc const
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
                     | ([Component_value.Ident "from", loc], _)
                     |  ([Component_value.Number "0", loc], _) ->
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
  let open Component_value in
  let rcv = render_component_value mode in
  let (name, name_loc) = d.Declaration.name in

  let render_box_shadow () =
    let box_shadow_ident =
      Exp.ident ~loc:name_loc { txt = Lident "boxShadow"; loc = name_loc } in
    let box_shadow_args grouped_param =
      let box_shadow_args_without_inset grouped_param loc =
        match grouped_param with
        | [cv_offset_x; cv_offset_y; cv_color] ->
          [(Labelled "x", rcv cv_offset_x);
           (Labelled "y", rcv cv_offset_y);
           (Nolabel, rcv cv_color);
          ]
        | [cv_offset_x; cv_offset_y; cv_blur_radius; cv_color] ->
          [(Labelled "x", rcv cv_offset_x);
           (Labelled "y", rcv cv_offset_y);
           (Labelled "blur", rcv cv_blur_radius);
           (Nolabel, rcv cv_color);
          ]
        | [cv_offset_x; cv_offset_y; cv_blur_radius; cv_spread_radius; cv_color] ->
          [(Labelled "x", rcv cv_offset_x);
           (Labelled "y", rcv cv_offset_y);
           (Labelled "blur", rcv cv_blur_radius);
           (Labelled "spread", rcv cv_spread_radius);
           (Nolabel, rcv cv_color);
          ]
        | _ -> grammar_error loc "Unexpected box-shadow parameter"
      in
      match grouped_param with
      | ((Ident "inset", inset_loc) :: rest, loc) ->
        (Labelled "inset",
         (Exp.construct ~loc:inset_loc
            { txt = Lident "true"; loc = inset_loc }
            None)) :: box_shadow_args_without_inset rest loc
      | (cvs, loc) ->
        box_shadow_args_without_inset cvs loc
    in
    let (params, _) = d.Declaration.value in
    let grouped_params = group_params params in
    let args =
      List.rev_map
        (fun params -> box_shadow_args params)
        grouped_params in
    let ident = Exp.ident ~loc:name_loc { txt = Lident "boxShadows"; loc = name_loc } in
    let box_shadow_list =
      List.map
        (fun arg -> Exp.apply box_shadow_ident arg)
        args in
    Exp.apply ident [(Nolabel, list_to_expr name_loc box_shadow_list)]
  in

  let render_text_shadow () =
    let text_shadow_args params loc =
      match params with
      | [cv_offset_x; cv_offset_y; cv_color] ->
        [(Labelled "x", rcv cv_offset_x);
         (Labelled "y", rcv cv_offset_y);
         (Nolabel, rcv cv_color);
        ]
      | [cv_offset_x; cv_offset_y; cv_blur_radius; cv_color] ->
        [(Labelled "x", rcv cv_offset_x);
         (Labelled "y", rcv cv_offset_y);
         (Labelled "blur", rcv cv_blur_radius);
         (Nolabel, rcv cv_color);
        ]
      | [cv_offset_x; cv_offset_y; cv_blur_radius; cv_spread_radius; cv_color] ->
        [(Labelled "x", rcv cv_offset_x);
         (Labelled "y", rcv cv_offset_y);
         (Labelled "blur", rcv cv_blur_radius);
         (Labelled "spread", rcv cv_spread_radius);
         (Nolabel, rcv cv_color);
        ]
      | _ -> grammar_error loc "Unexpected text-shadow parameter"
    in
    let (params, loc) = d.Declaration.value in
    let args = text_shadow_args params loc in
    let ident = Exp.ident ~loc:name_loc { txt = Lident "textShadow"; loc = name_loc } in
    Exp.apply ident args
  in

  let render_transition () =
    let transition_ident =
      Exp.ident ~loc:name_loc { txt = Lident "transition"; loc = name_loc } in
    let render_property property loc =
      Exp.constant ~loc (Const.string property)
    in
    let transition_args (grouped_param, loc) =
      match grouped_param with
      | [(Ident property, p_loc)] ->
        [(Nolabel, render_property property p_loc);
        ]
      | [(Ident property, p_loc);
         (Dimension _, _) as cv_duration] ->
        [(Labelled "duration", rcv cv_duration);
         (Nolabel, render_property property p_loc);
        ]
      | [(Ident property, p_loc);
         (Dimension _, _) as cv_duration;
         (Dimension _, _) as cv_delay] ->
        [(Labelled "duration", rcv cv_duration);
         (Labelled "delay", rcv cv_delay);
         (Nolabel, render_property property p_loc);
        ]
      | [(Ident property, loc);
         (Dimension _, _) as cv_duration;
         (Dimension _, _) as cv_delay;
         (Ident _, _) as cv_timing_function]
      | [(Ident property, loc);
         (Dimension _, _) as cv_duration;
         (Dimension _, _) as cv_delay;
         (Function _, _) as cv_timing_function] ->
        [(Labelled "duration", rcv cv_duration);
         (Labelled "delay", rcv cv_delay);
         (Labelled "timingFunction", rcv cv_timing_function);
         (Nolabel, render_property property loc);
        ]
      | _ -> grammar_error loc "Unexpected transition parameter"
    in
    let (params, _) = d.Declaration.value in
    let grouped_params = group_params params in
    let args =
      List.rev_map
        (fun params -> transition_args params)
        grouped_params in
    let ident = Exp.ident ~loc:name_loc { txt = Lident "transitions"; loc = name_loc } in
    let transition_list =
      List.map
        (fun arg -> Exp.apply transition_ident arg)
        args in
    Exp.apply ident [(Nolabel, list_to_expr name_loc transition_list)]
  in

  let render_standard_declaration () =
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
      List.map (fun v -> rcv v) vs in
    Exp.apply ~loc:d_loc ident (List.map (fun a -> (Nolabel, a)) args)
  in

  let render_transform () =
    let (vs, loc) = d.Declaration.value in
    if List.length vs = 1 then
      render_standard_declaration ()
    else
      let cvs = List.rev_map (fun v -> rcv v) vs in
      let arg = list_to_expr loc cvs in
      let ident = Exp.ident ~loc:name_loc { txt = Lident "transforms"; loc = name_loc } in
      Exp.apply ~loc:d_loc ident [Nolabel, arg]
  in

  match name with
  | "box-shadow" ->
    render_box_shadow ()
  | "text-shadow" ->
    render_text_shadow ()
  | "transform" ->
    render_transform ()
  | "transition" ->
    render_transition ()
  | _ ->
    render_standard_declaration ()

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
