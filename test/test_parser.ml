open Ppx_bs_css
open Css_types

let rec zip xs ys =
  match (xs, ys) with
  | ([], []) -> []
  | (x :: xs, y :: ys) -> (x, y) :: (zip xs ys)
  | _ -> failwith "zip"

let eq_ast ast1 ast2 =
  let eq_list xs ys eq =
    List.fold_left
      (fun e (x, y) -> e && eq x y)
      true
      (zip xs ys)
  in
  let rec eq_component_value (cv1, cv1_loc) (cv2, cv2_loc) =
    let open Component_value in
    match (cv1, cv2) with
    | (Brace_block b1, Brace_block b2)
    | (Paren_block b1, Paren_block b2)
    | (Bracket_block b1, Bracket_block b2) ->
      eq_list b1 b2 eq_component_value
    | (Percentage x1, Percentage x2)
    | (Ident x1, Ident x2)
    | (String x1, String x2)
    | (Uri x1, Uri x2)
    | (Operator x1, Operator x2)
    | (Delim x1, Delim x2)
    | (Hash x1, Hash x2)
    | (Number x1, Number x2)
    | (Unicode_range x1, Unicode_range x2) ->
      x1 = x2
    | (Float_dimension x1, Float_dimension x2)
    | (Dimension x1, Dimension x2) ->
      x1 = x2
    | (At_rule r1, At_rule r2) ->
      eq_at_rule r1 r2
    | (Function ((n1, _), b1), Function ((n2, _), b2)) ->
      n1 = n2 &&
      eq_list b1 b2 eq_component_value
    | _ -> false
  and eq_at_rule r1 r2 =
    let (n1, _) = r1.At_rule.name in
    let (n2, _) = r2.At_rule.name in
    let (pr1, _) = r1.At_rule.prelude in
    let (pr2, _) = r2.At_rule.prelude in
    (n1 = n2) &&
    (eq_list pr1 pr2 eq_component_value) &&
    begin match (r1.At_rule.block, r2.At_rule.block) with
      | (None, None) -> true
      | (Some b1, Some b2) -> eq_component_value b1 b2
      | _ -> false
    end
  in
  let eq_declaration d1 d2 =
    let (n1, _) = d1.Declaration.name in
    let (n2, _) = d2.Declaration.name in
    let (v1, _) = d1.Declaration.value in
    let (v2, _) = d2.Declaration.value in
    let (i1, _) = d1.Declaration.important in
    let (i2, _) = d2.Declaration.important in
    (n1 = n2) &&
    (eq_list v1 v2 eq_component_value) &&
    (i1 = i2)
  in
  let eq_declaration_list (dl1, dl1_loc) (dl2, dl2_loc) =
    let eq_kind k1 k2 =
      match (k1, k2) with
      | (Declaration_list.Declaration d1, Declaration_list.Declaration d2) ->
        eq_declaration d1 d2
      | (Declaration_list.At_rule r1, Declaration_list.At_rule r2) ->
        eq_at_rule r1 r2
      | _ -> false
    in
    eq_list dl1 dl2 eq_kind
  in
  let eq_style_rule r1 r2 =
    let (pr1, _) = r1.Style_rule.prelude in
    let (pr2, _) = r2.Style_rule.prelude in
    (eq_list pr1 pr2 eq_component_value) &&
    (eq_declaration_list r1.Style_rule.block r2.Style_rule.block)
  in
  let eq_rule r1 r2 =
    match (r1, r2) with
    | (Rule.Style_rule r1, Rule.Style_rule r2) -> eq_style_rule r1 r2
    | (Rule.At_rule r1, Rule.At_rule r2) -> eq_at_rule r1 r2
    | _ -> false
  in
  let eq_stylesheet (st1, st1_loc) (st2, st2_loc) =
    eq_list st1 st2 eq_rule
  in
  eq_stylesheet ast1 ast2

let parse_stylesheet css =
  try Css_lexer.parse_string css Css_parser.stylesheet with
  | Css_lexer.LexingError (pos, msg) ->
    failwith
      ("Lexing error at: " ^ Css_lexer.position_to_string pos)
  | Css_lexer.ParseError (token, start, finish) ->
    failwith
      (Printf.sprintf "Parsing error: Unexpected token=%s start=%s end=%s"
         (Css_lexer.token_to_string token)
         (Css_lexer.position_to_string start)
         (Css_lexer.position_to_string finish))

let test_stylesheet_parser () =
  let css =
    {|
{
  /* This is a comment */
  color: red !important; /* This is another comment */
  background-color: red;
}

p q r {
}
|}
  in
  let ast = parse_stylesheet css in
  let expected_ast =
    ([ Rule.Style_rule
         { Style_rule.prelude = ([], Location.none);
           block =
             ([ Declaration_list.Declaration
                  { Declaration.name = ("color", Location.none);
                    value = ([(Component_value.Ident "red", Location.none)], Location.none);
                    important = (true, Location.none);
                    loc = Location.none;
                  };
                Declaration_list.Declaration
                  { Declaration.name = ("background-color", Location.none);
                    value = ([(Component_value.Ident "red", Location.none)], Location.none);
                    important = (false, Location.none);
                    loc = Location.none;
                  }
              ], Location.none);
           loc = Location.none;
         };
       Rule.Style_rule
         { Style_rule.prelude = (
               [ (Component_value.Ident "p", Location.none);
                 (Ident "q", Location.none);
                 (Ident "r", Location.none)
               ], Location.none);
           block = ([], Location.none);
           loc = Location.none;
         }
     ], Location.none)
  in
  Alcotest.(check (testable Css_fmt_printer.dump_stylesheet eq_ast))
    "different CSS AST" expected_ast ast

let test_css_functions () =
  let css =
    {|
{
  color: rgb(1, 2, 3);
}
|}
  in
  let ast = parse_stylesheet css in
  let expected_ast =
    ([ Rule.Style_rule
         { Style_rule.prelude = ([], Location.none);
           block =
             ([ Declaration_list.Declaration
                  { Declaration.name = ("color", Location.none);
                    value = (
                      [ (Component_value.Function (
                            ("rgb", Location.none),
                            ([ (Component_value.Number "1", Location.none);
                               (Component_value.Delim ",", Location.none);
                               (Component_value.Number "2", Location.none);
                               (Component_value.Delim ",", Location.none);
                               (Component_value.Number "3", Location.none);
                             ])),
                         Location.none);
                      ],
                      Location.none);
                    important = (false, Location.none);
                    loc = Location.none;
                  };
              ], Location.none);
           loc = Location.none;
         };
     ], Location.none)
  in
  Alcotest.(check (testable Css_fmt_printer.dump_stylesheet eq_ast))
    "different CSS AST" expected_ast ast

let test_set =
  [("CSS parser", `Quick, test_stylesheet_parser);
   ("CSS functions", `Quick, test_css_functions);
  ]
