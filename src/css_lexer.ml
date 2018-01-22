module Sedlexing = Lex_buffer

exception LexingError of (Lexing.position * string)
(** Signals a lexing error at the provided source location.  *)

exception ParseError of (Css_parser.token * Lexing.position * Lexing.position)
(** Signals a parsing error at the provided token and its start and end
 * locations. *)

let token_to_string = function
  | Css_parser.STRING s -> "STRING(" ^ s ^ ")"
  | SEMI_COLON -> ";"
  | RIGHT_BRACE -> "}"
  | LEFT_BRACE -> "{"
  | EOF -> "EOF"
  | COLON -> ":"


let position_to_string position =
  Printf.sprintf "l%dc%d" position.Lexing.pos_lnum
    (position.Lexing.pos_cnum - position.Lexing.pos_bol)


let () =
  Location.register_error_of_exn (function
      | LexingError (pos, msg) ->
        let loc = {Location.loc_start= pos; loc_end= pos; loc_ghost= false} in
        Some {loc; msg; sub= []; if_highlight= ""}
      | ParseError (token, loc_start, loc_end) ->
        let loc = {Location.loc_start; loc_end; loc_ghost= false} in
        let msg =
          Printf.sprintf "Parse error while reading token '%s'"
            (token_to_string token)
        in
        Some {loc; msg; sub= []; if_highlight= ""}
      | _ -> None )


let newline = [%sedlex.regexp ? '\n' | "\r\n" | '\r' | '\012']

let white_space = [%sedlex.regexp ? " " | '\t' | newline]

let hex_digit = [%sedlex.regexp ? '0'..'9' | 'a'..'f' | 'A'..'F']

let discard_comments_and_white_spaces buf =
  let rec discard_white_spaces buf =
    [%sedlex
      match buf with
      | Plus white_space -> discard_white_spaces buf
      | "/*" -> discard_comments buf
      | _ -> ()]
  and discard_comments buf =
    [%sedlex
      match buf with
      | eof ->
        raise
          (LexingError (buf.Lex_buffer.pos, "Unterminated comment at EOF"))
      | "*/" -> discard_white_spaces buf
      | any -> discard_comments buf
      | _ -> assert false]
  in
  discard_white_spaces buf


let get_next_token buf =
  discard_comments_and_white_spaces buf ;
  [%sedlex
    match buf with
    | ';' -> Css_parser.SEMI_COLON
    | '}' -> Css_parser.RIGHT_BRACE
    | '{' -> Css_parser.LEFT_BRACE
    | eof -> Css_parser.EOF
    | ':' -> Css_parser.COLON
    | xid_start, Star (xid_continue | '-') ->
      Css_parser.STRING (Lex_buffer.ascii buf)
    | _ ->
      let msg =
        Printf.sprintf "Unexpected character in expression: '%s'"
          (Char.escaped (Char.chr (Lex_buffer.next buf)))
      in
      raise (LexingError (buf.Lex_buffer.pos, msg))]


let get_next_token_with_location buf =
  discard_comments_and_white_spaces buf ;
  let loc_start = Lex_buffer.next_loc buf in
  let token = get_next_token buf in
  let loc_end = Lex_buffer.next_loc buf in
  (token, loc_start, loc_end)


let parse buf p =
  let last_token = ref (Css_parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos) in
  let next_token () =
    last_token := get_next_token_with_location buf ;
    !last_token
  in
  try MenhirLib.Convert.Simplified.traditional2revised p next_token with
  | LexingError (pos, s) as e -> raise e
  | _ -> raise (ParseError !last_token)


let parse_string ?pos s p = parse (Lex_buffer.of_ascii_string ?pos s) p
