let declaration_to_string (declaration: Css_types.Declaration.t) =
  Css_types.Declaration.(declaration.property ^ ": " ^ declaration.value)

let block_to_string (block: Css_types.Block.t) =
  let body = block
             |> List.map declaration_to_string
             |> String.concat ";\n\t"
  in
  "{\n\t" ^ body ^ "\n}\n\n"

let rule_to_string (rule: Css_types.Rule.t) =
  let prelude = String.concat " " rule.Css_types.Rule.prelude in
  prelude ^ " " ^ (block_to_string rule.Css_types.Rule.block)

let stylesheet_to_string (stylesheet: Css_types.Stylesheet.t) =
  List.fold_left
    (fun s rule -> s ^ (rule_to_string rule))
    ""
    stylesheet
