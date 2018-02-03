let rec dump_component_value ppf (cv: Ppx_bs_css.Css_types.Component_value.t) =
  let dump_block start_char end_char cs =
    let pp = Fmt.(vbox ~indent:2 (list ~sep:((const string ";") |> suffix cut) dump_component_value |> prefix cut)) in
    let pp = pp
      |> Fmt.(prefix (const string start_char))
      |> Fmt.(suffix cut)
      |> Fmt.(suffix (const string end_char)) in
    pp ppf cs
  in
  match cv with
  | Ppx_bs_css.Css_types.Component_value.Brace_block cs ->
    dump_block "{" "}" cs
  | Paren_block cs ->
    dump_block "(" ")" cs
  | Bracket_block cs ->
    dump_block "[" "]" cs
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
  | At_rule ar ->
    dump_at_rule ppf ar
  | Function (name, params) ->
    let pp_name = Fmt.string |> Fmt.(suffix (const string "(")) in
    let pp_params = Fmt.(list ~sep:(const string ", ") dump_component_value)
      |> Fmt.(suffix (const string ")")) in
    let pp = Fmt.pair ~sep:Fmt.nop pp_name pp_params in
    pp ppf (name, params)
  | Dimension (number, dimension) ->
    let pp = Fmt.fmt "%s%s" in
    pp ppf number dimension
and dump_at_rule ppf (ar: Ppx_bs_css.Css_types.At_rule.t) =
  let pp_name = Fmt.string |> Fmt.(prefix (const string "@")) in
  let pp_prelude = Fmt.(list ~sep:(const string ", ") dump_component_value) in
  let pp_block = Fmt.(option ~none:(const string ";") dump_component_value) in
  Fmt.fmt "%a %a %a"
    ppf
    pp_name ar.Ppx_bs_css.Css_types.At_rule.name
    pp_prelude ar.Ppx_bs_css.Css_types.At_rule.prelude
    pp_block ar.Ppx_bs_css.Css_types.At_rule.block

let dump_declaration ppf (d: Ppx_bs_css.Css_types.Declaration.t) =
  let pp_name = Fmt.string in
  let pp_values = Fmt.(list ~sep:(const string " ") dump_component_value) in
  let pp_important =
    match d.Ppx_bs_css.Css_types.Declaration.important with
    | false -> Fmt.nop
    | true -> Fmt.(const string " !important") in
  Fmt.fmt "%a: %a%a"
    ppf
    pp_name d.Ppx_bs_css.Css_types.Declaration.name
    pp_values d.Ppx_bs_css.Css_types.Declaration.value
    pp_important ()

let dump_declaration_list ppf (dl: Ppx_bs_css.Css_types.Declaration_list.t) =
  let pp_elem ppf d =
  match d with
  | Ppx_bs_css.Css_types.Declaration_list.At_rule ar -> dump_at_rule ppf ar
  | Ppx_bs_css.Css_types.Declaration_list.Declaration d -> dump_declaration ppf d
  in
  let pp = Fmt.(vbox ~indent:2 (list ~sep:((const string ";") |> suffix cut) pp_elem |> prefix cut)) in
  pp ppf dl

let dump_style_rule ppf (sr: Ppx_bs_css.Css_types.Style_rule.t) =
  let pp_prelude = Fmt.(list ~sep:(const string ", ") dump_component_value) in
  Fmt.fmt "%a {@,%a@,}@,"
    ppf
    pp_prelude sr.Ppx_bs_css.Css_types.Style_rule.prelude
    dump_declaration_list sr.Ppx_bs_css.Css_types.Style_rule.block

let dump_rule ppf (r: Ppx_bs_css.Css_types.Rule.t) =
  match r with
  | Ppx_bs_css.Css_types.Rule.Style_rule sr -> dump_style_rule ppf sr
  | Ppx_bs_css.Css_types.Rule.At_rule ar -> dump_at_rule ppf ar

let dump_stylesheet ppf (s: Ppx_bs_css.Css_types.Stylesheet.t) =
  let pp = Fmt.(vbox (list dump_rule)) in
  pp ppf s
