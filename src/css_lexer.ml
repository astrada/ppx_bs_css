(** CSS lexer.
  * Reference:
  * https://www.w3.org/TR/css-syntax-3/
  * https://github.com/yahoo/css-js/blob/master/src/l/css.3.l *)

module Sedlexing = Lex_buffer

exception LexingError of (Lexing.position * string)
(** Signals a lexing error at the provided source location.  *)

exception ParseError of (Css_parser.token * Lexing.position * Lexing.position)
(** Signals a parsing error at the provided token and its start and end
 * locations. *)

let token_to_string = function
  | Css_parser.EOF -> "EOF"
  | SEMI_COLON -> ";"
  | RIGHT_BRACE -> "}"
  | LEFT_BRACE -> "{"
  | COLON -> ":"
  | RIGHT_PAREN -> ")"
  | PERCENTAGE -> "%"
  | OPERATOR s -> "OPERATOR(" ^ s ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | URI s -> "URI(" ^ s ^ ")"
  | IMPORTANT -> "!important"
  | AT_RULE s -> "AT_RULE(" ^ s ^ ")"
  | UNICODE_RANGE s -> "UNICODE_RANGE(" ^ s ^ ")"
  | FUNCTION s -> "FUNCTION(" ^ s ^ ")"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | HASH s -> "HASH(" ^ s ^ ")"
  | NUMBER s -> "NUMBER(" ^ s ^ ")"
  | DIMENSION (n, d) -> "DIMENSION(" ^ n ^ ", " ^ d ^ ")"
  | DELIM s -> "DELIM(" ^ s ^ ")"


let position_to_string pos =
  Printf.sprintf "line:%d offset:%d"
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)


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

(* Regexes *)
let newline = [%sedlex.regexp? '\n' | "\r\n" | '\r' | '\012']

let white_space = [%sedlex.regexp? " " | '\t' | newline]

let ws = [%sedlex.regexp? Star white_space]

let hex_digit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']

let digit = [%sedlex.regexp? '0'..'9']

let non_ascii = [%sedlex.regexp? '\o240'..'\o377']

let up_to_6_hex_digits = [%sedlex.regexp? Rep (hex_digit, 1 .. 6)]

let unicode =  [%sedlex.regexp? '\\', up_to_6_hex_digits, Opt white_space]

let unicode_range = [%sedlex.regexp? Rep ((hex_digit | '?'), 1 .. 6) | (up_to_6_hex_digits , '-', up_to_6_hex_digits)]

let escape = [%sedlex.regexp? unicode | '\\', Compl ('\r' | '\n' | '\012' | hex_digit)]

let ident_start = [%sedlex.regexp? ('_' | 'a'..'z' | 'A'..'Z') | non_ascii | escape]

let ident_char = [%sedlex.regexp? ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9'| '-') | non_ascii | escape]

let ident = [%sedlex.regexp? Opt ('-'), ident_start, Star ident_char]

let string_quote = [%sedlex.regexp? ('"', Star (Compl ('\n' | '\r' | '\012' | '"') | '\\', newline | escape), '"')]

let string_apos = [%sedlex.regexp? ('\'', Star (Compl ('\n' | '\r' | '\012' | '\'') | '\\', newline | escape), '\'')]

let string = [%sedlex.regexp? string_quote | string_apos]

let name = [%sedlex.regexp? Plus ident_char]

let number = [%sedlex.regexp? (Plus digit, Opt ('.', Plus digit), Opt (('e' | 'E'), ('+' | '-'), Plus digit))
                          | ('.', Plus digit, Opt (('e' | 'E'), ('+' | '-'), Plus digit))]

let non_printable = [%sedlex.regexp? '\x00'..'\x08' | '\x0B' | '\x0E'..'\x1F' | '\x7F']

let url_unquoted = [%sedlex.regexp? Star (Compl ('"' | '\'' | '(' | ')' | '\\' | non_printable) | escape)]

let url = [%sedlex.regexp? url_unquoted | string]

let operator = [%sedlex.regexp? "~=" | "|=" | "^=" | "$=" | "*=" | "||"]

let at_rule = [%sedlex.regexp? "@", ident]

let _a = [%sedlex.regexp? 'A' | 'a']
let _b = [%sedlex.regexp? 'B' | 'b']
let _c = [%sedlex.regexp? 'C' | 'c']
let _d = [%sedlex.regexp? 'D' | 'd']
let _e = [%sedlex.regexp? 'e' | 'E']
let _f = [%sedlex.regexp? 'F' | 'f']
let _g = [%sedlex.regexp? 'G' | 'g']
let _h = [%sedlex.regexp? 'H' | 'h']
let _i = [%sedlex.regexp? 'I' | 'i']
let _j = [%sedlex.regexp? 'J' | 'j']
let _k = [%sedlex.regexp? 'K' | 'k']
let _l = [%sedlex.regexp? 'L' | 'l']
let _m = [%sedlex.regexp? 'M' | 'm']
let _n = [%sedlex.regexp? 'N' | 'n']
let _o = [%sedlex.regexp? 'O' | 'o']
let _p = [%sedlex.regexp? 'P' | 'p']
let _q = [%sedlex.regexp? 'Q' | 'q']
let _r = [%sedlex.regexp? 'R' | 'r']
let _s = [%sedlex.regexp? 'S' | 's']
let _t = [%sedlex.regexp? 'T' | 't']
let _u = [%sedlex.regexp? 'U' | 'u']
let _v = [%sedlex.regexp? 'V' | 'v']
let _w = [%sedlex.regexp? 'W' | 'w']
let _x = [%sedlex.regexp? 'X' | 'x']
let _y = [%sedlex.regexp? 'Y' | 'y']
let _z = [%sedlex.regexp? 'Z' | 'z']

let important = [%sedlex.regexp? "!", ws, _i, _m, _p, _o, _r, _t, _a, _n, _t]

let dimension = [%sedlex.regexp?
    (* length *)
    (_c, _a, _p)
                             | (_c, _h)
                             | (_e, _m)
                             | (_e, _x)
                             | (_i, _c)
                             | (_l, _h)
                             | (_r, _e, _m)
                             | (_r, _l, _h)
                             | (_v, _h)
                             | (_p, _x)
                             | (_m, _m)
                             | _q
                             | (_c, _m)
                             | (_i, _n)
                             | (_p, _t)
                             | (_p, _c)
                             (* angle *)
                             | (_d, _e, _g)
                             | (_g, _r, _a, _d)
                             | (_r, _a, _d)
                             | (_t, _u, _r, _n)
                             (* time *)
                             | (_m, _s)
                             | _s
                             (* frequency *)
                             | (_h, _z)
                             | (_k, _h, _z)
]

let discard_comments_and_white_spaces buf =
  let rec discard_white_spaces buf =
    match%sedlex buf with
    | Plus white_space -> discard_white_spaces buf
    | "/*" -> discard_comments buf
    | _ -> ()
  and discard_comments buf =
    match%sedlex buf with
    | eof ->
      raise
        (LexingError (buf.Lex_buffer.pos, "Unterminated comment at EOF"))
    | "*/" -> discard_white_spaces buf
    | any -> discard_comments buf
    | _ -> assert false
  in
  discard_white_spaces buf

let rec get_next_token buf =
  discard_comments_and_white_spaces buf ;
  match%sedlex buf with
  | eof -> Css_parser.EOF
  | ';' -> Css_parser.SEMI_COLON
  | '}' -> Css_parser.RIGHT_BRACE
  | '{' -> Css_parser.LEFT_BRACE
  | ':' -> Css_parser.COLON
  | ')' -> Css_parser.RIGHT_PAREN
  | '%' -> Css_parser.PERCENTAGE
  | operator -> Css_parser.OPERATOR (Lex_buffer.latin1 buf)
  | string -> Css_parser.STRING (Lex_buffer.latin1 buf)
  | "url(", ws, url, ws, ")" -> Css_parser.URI (Lex_buffer.latin1 buf)
  | important -> Css_parser.IMPORTANT
  | at_rule -> Css_parser.AT_RULE (Lex_buffer.latin1 buf)
  (* NOTE: should be placed above ident, otherwise pattern with
   * '-[0-9a-z]{1,6}' cannot be matched *)
  | _u, '+', unicode_range -> Css_parser.UNICODE_RANGE (Lex_buffer.latin1 buf)
  | ident, '(' -> Css_parser.FUNCTION (Lex_buffer.latin1 buf)
  | ident -> Css_parser.IDENT (Lex_buffer.latin1 buf)
  | '#', name -> Css_parser.HASH (Lex_buffer.latin1 buf)
  | number -> get_dimension (Lex_buffer.latin1 buf) buf
  | any -> Css_parser.DELIM (Lex_buffer.latin1 buf)
  | _ -> assert false
and get_dimension n buf =
  match%sedlex buf with
  | dimension
  | ident ->
    Css_parser.DIMENSION (n, Lex_buffer.latin1 buf)
  | any ->
    Css_parser.NUMBER (n)
  | _ -> assert false


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
