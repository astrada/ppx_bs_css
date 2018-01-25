let dump_declaration ppf (declaration: Ppx_bs_css.Css_types.Declaration.t) =
  let pp = Fmt.(pair ~sep:(const string ": ") string string)
           |> Fmt.(prefix cut)
  in
  pp ppf
    Ppx_bs_css.Css_types.Declaration.(declaration.property, declaration.value)

let dump_block ppf (block: Ppx_bs_css.Css_types.Block.t) =
  let pp =
    Fmt.(vbox ~indent:1 (list ~sep:(const string ";") dump_declaration))
    |> Fmt.(prefix (const string "{"))
  in
  let pp = pp
           |> Fmt.(suffix cut)
           |> Fmt.(suffix (const string "}"))
           |> Fmt.(suffix cut)
  in
  pp ppf block

let dump_rule ppf (rule: Ppx_bs_css.Css_types.Rule.t) =
  let dump_prelude = Fmt.hbox (Fmt.list Fmt.string) in
  Fmt.(pair ~sep:(const string " ") dump_prelude dump_block) ppf
    Ppx_bs_css.Css_types.Rule.(rule.prelude, rule.block)

let dump_stylesheet ppf (stylesheet: Ppx_bs_css.Css_types.Stylesheet.t) =
  Fmt.(vbox (list dump_rule)) ppf stylesheet

