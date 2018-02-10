open Ppx_bs_css
open Css_types

let rec dump_component_value ppf (cv, loc) =
  let dump_block start_char end_char cs =
    let pp =
      let open Fmt in
      vbox ~indent:2
        ( list ~sep:(const string ";" |> suffix cut) dump_component_value
          |> prefix cut )
    in
    let pp =
      pp |> Fmt.(prefix (const string start_char)) |> Fmt.(suffix cut)
      |> Fmt.(suffix (const string end_char))
    in
    pp ppf cs
  in
  match cv with
  | Component_value.Brace_block cs ->
    dump_block "{" "}" cs
  | Paren_block cs -> dump_block "(" ")" cs
  | Bracket_block cs -> dump_block "[" "]" cs
  | Percentage p ->
    let pp = Fmt.string |> Fmt.(suffix (const string "%")) in
    pp ppf p
  | Ident s
  | String s
  | Uri s
  | Operator s
  | Delim s
  | Hash s
  | Number s
  | Unicode_range s ->
    let pp = Fmt.string in
    pp ppf s
  | At_rule ar -> dump_at_rule ppf ar
  | Function ((name, name_loc), params) ->
    let pp_name = Fmt.string |> Fmt.(suffix (const string "(")) in
    let pp_params =
      Fmt.(list ~sep:(const string ", ") dump_component_value)
      |> Fmt.(suffix (const string ")"))
    in
    let pp = Fmt.pair ~sep:Fmt.nop pp_name pp_params in
    pp ppf (name, params)
  | Float_dimension (number, dimension)
  | Dimension (number, dimension) ->
    let pp = Fmt.fmt "%s%s" in
    pp ppf number dimension

and dump_at_rule ppf (ar: At_rule.t) =
  let pp_name = Fmt.string |> Fmt.(prefix (const string "@")) in
  let pp_prelude = Fmt.(list ~sep:(const string " ") dump_component_value) in
  let pp_block = Fmt.(option ~none:(const string ";") dump_component_value) in
  let (name, name_loc) = ar.At_rule.name in
  let (prelude, prelude_loc) = ar.At_rule.prelude in
  Fmt.fmt "%a %a %a" ppf pp_name name
    pp_prelude prelude pp_block
    ar.At_rule.block

let dump_declaration ppf (d: Declaration.t) =
  let pp_name = Fmt.string in
  let pp_values = Fmt.(list ~sep:(const string " ") dump_component_value) in
  let pp_important =
    match d.Declaration.important with
    | (false, _) -> Fmt.nop
    | (true, _) -> Fmt.(const string " !important")
  in
  let (name, name_loc) = d.Declaration.name in
  let (value, value_loc) = d.Declaration.value in
  Fmt.fmt "%a: %a%a" ppf pp_name name
    pp_values value pp_important ()

let dump_declaration_list ppf (dl, dl_loc) =
  let pp_elem ppf d =
    match d with
    | Declaration_list.At_rule ar -> dump_at_rule ppf ar
    | Declaration_list.Declaration d ->
      dump_declaration ppf d
  in
  let pp =
    let open Fmt in
    vbox ~indent:2
      (list ~sep:(const string ";" |> suffix cut) pp_elem |> prefix cut)
  in
  pp ppf dl

let dump_style_rule ppf (sr: Style_rule.t) =
  let pp_prelude = Fmt.(list ~sep:(const string " ") dump_component_value) in
  let (prelude, prelude_loc) = sr.Style_rule.prelude in
  Fmt.fmt "%a {@,%a@,}@," ppf pp_prelude
    prelude dump_declaration_list
    sr.Style_rule.block

let dump_rule ppf (r: Rule.t) =
  match r with
  | Rule.Style_rule sr -> dump_style_rule ppf sr
  | Rule.At_rule ar -> dump_at_rule ppf ar

let dump_stylesheet ppf (s, s_loc) =
  let pp = Fmt.(vbox (list dump_rule)) in
  pp ppf s
