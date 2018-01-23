
let test_stylesheet_parser () =
  let css = "{
  /* This is a comment */
  color: red; /* This is another comment */
  backgroun-color: red;
}

p {
}"
  in
  let ast = Ppx_bs_css.(Css_lexer.parse_string css Css_parser.stylesheet) in
  let expected = " {
\tcolor: red;
\tbackgroun-color: red
}

p {
\t
}

" in
  Alcotest.(check string)
    "different CSS content"
    expected
    (Ppx_bs_css.Css_printer.stylesheet_to_string ast)

let test_set = [
  "CSS parser", `Quick, test_stylesheet_parser;
]

let () =
  Alcotest.run "Test suite" [
    "test_set", test_set;
  ]
