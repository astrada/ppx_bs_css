let test_stylesheet_parser () =
  let css =
    {|
{
  /* This is a comment */
  color: red; /* This is another comment */
  background-color: red;
}

p q r {
}
|}
  in
  let ast =
    try
      Ppx_bs_css.(Css_lexer.parse_string css Css_parser.stylesheet)
    with
    | Ppx_bs_css.Css_lexer.LexingError(pos, msg) ->
      failwith ("Lexing error at: " ^ (Ppx_bs_css.Css_lexer.position_to_string pos))
    | Ppx_bs_css.Css_lexer.ParseError(token, start, finish) ->
      failwith (Printf.sprintf "Parsing error: Unexpected token=%s start=%s end=%s"
                  (Ppx_bs_css.Css_lexer.token_to_string token)
                  (Ppx_bs_css.Css_lexer.position_to_string start)
                  (Ppx_bs_css.Css_lexer.position_to_string finish))
  in
  let expected_ast =
    let open Ppx_bs_css.Css_types in
    [ { Rule.prelude= []
      ; block=
          (let open Declaration in
           [ {property= "color"; value= "red"}
           ; {property= "background-color"; value= "red"} ]) }
    ; {Rule.prelude= ["p"; "q"; "r"]; block= []} ]
  in
  Alcotest.(check (of_pp Css_printer.dump_stylesheet))
    "different CSS AST" expected_ast ast


let test_set = [("CSS parser", `Quick, test_stylesheet_parser)]

let () = Alcotest.run "Test suite" [("test_set", test_set)]
