let stylesheet = [%css "{
  /* This is a comment */
  color: red; /* This is another comment */
  backgroun-color: red;
}

p {
}"]

let () =
  print_endline stylesheet
