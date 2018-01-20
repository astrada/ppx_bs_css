module Sedlexing = Lex_buffer

(** Signals a lexing error at the provided source location.  *)
exception LexingError of (Lexing.position * string)

(** Signals a parsing error at the provided token and its start and end
 * locations. *)
exception ParseError of (Css_parser.token * Lexing.position * Lexing.position)

let token_to_string = function
  | Css_parser.STRING s -> "STRING(" ^ s ^ ")"
  | SEMI_COLON -> ";"
  | RIGHT_BRACE -> "}"
  | LEFT_BRACE -> "{"
  | EOF -> "EOF"
  | COLON -> ":"

let position_to_string position =
  Printf.sprintf "l%dc%d"
    position.Lexing.pos_lnum
    (position.Lexing.pos_cnum - position.Lexing.pos_bol)
    
let _ =
  Location.register_error_of_exn (
    function
      | LexingError (pos, msg) ->
        let loc =
          { Location.loc_start = pos; loc_end = pos; loc_ghost = false } in
        Some { loc; msg; sub=[]; if_highlight="" }
      | ParseError (token, loc_start, loc_end) ->
        let loc = { Location.loc_start; loc_end; loc_ghost = false } in
        let msg =
          Printf.sprintf "Parse error while reading token '%s'"
            (token_to_string token) in
        Some { loc; msg; sub=[]; if_highlight=""; }
      | _ -> None
  )

let discard_comments_and_white_spaces buf =
  let rec discard_white_spaces buf =
    match%sedlex buf with
    | Plus white_space -> discard_white_spaces buf
    | "/*" -> discard_comments buf
    | _ -> ()
  and discard_comments buf =
    match%sedlex buf with
    | eof -> raise (LexingError (buf.Lex_buffer.pos, "Unterminated comment at EOF"))
    | "*/" -> discard_white_spaces buf;
    | any -> discard_comments buf
    | _ -> assert false
  in
  Printf.printf "comments/white-spaces: start=%s\n%!"
    (position_to_string buf.Lex_buffer.pos);
  discard_white_spaces buf;
  Printf.printf "comments/white-spaces: end=%s\n%!"
    (position_to_string buf.Lex_buffer.pos)

let get_next_token buf =
  Printf.printf "token: start=%s\n%!"
    (position_to_string buf.Lex_buffer.pos);
  discard_comments_and_white_spaces buf;
  let res =
  match%sedlex buf with
  | ';' -> Css_parser.SEMI_COLON
  | '}' -> Css_parser.RIGHT_BRACE
  | '{' -> Css_parser.LEFT_BRACE
  | eof -> Css_parser.EOF
  | ':' -> Css_parser.COLON
  | xid_start, (Star (xid_continue | '-')) ->
    Css_parser.STRING (Lex_buffer.ascii buf)
  | _ ->
    let msg =
      Printf.sprintf "Unexpected character in expression: '%s'"
        (Char.escaped (Char.chr (Lex_buffer.next buf))) in
    raise (LexingError (buf.Lex_buffer.pos, msg)) in
  Printf.printf "token: end=%s\n%!"
    (position_to_string buf.Lex_buffer.pos);
  res

let get_next_token_with_location buf =
  discard_comments_and_white_spaces buf;
  let loc_start = Lex_buffer.next_loc buf in
  let token = get_next_token buf in
  let loc_end = Lex_buffer.next_loc buf in
  (token, loc_start, loc_end)

let parse buf p =
  let last_token = ref (Css_parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos) in
  let next_token () =
    last_token := get_next_token_with_location buf;
    !last_token
  in
  try MenhirLib.Convert.Simplified.traditional2revised p next_token with
  | (LexingError (pos, s)) as e -> raise e
  | _ -> raise (ParseError (!last_token))

let parse_string ?pos s p =
  parse (Lex_buffer.of_ascii_string ?pos s) p

