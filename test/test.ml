let declarations_block = [%css "{
  /* This is a comment */
  color: red; /* This is another comment */
  backgroun-color: red;
}"]

let () =
  print_endline declarations_block

