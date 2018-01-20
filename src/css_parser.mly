%token <string> STRING
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMI_COLON
%token COLON
%token EOF

%start <Css_types.Block.t> block_eof

%%

block_eof:
  b = block; EOF { b }
  ;

block:
  | LEFT_BRACE; ds = declarations; RIGHT_BRACE { List.rev ds }
  ;

declarations_no_trailer:
  | d = declaration { [d] }
  | ds = declarations_no_trailer; SEMI_COLON; d = declaration { d :: ds }
  ;

declarations:
  | ds = declarations_no_trailer { ds }
  | ds = declarations_no_trailer; SEMI_COLON { ds }
  ;

declaration:
  p = STRING; COLON; v = value { { Css_types.Declaration.property = p; value = v } }
  ;

value:
  v = STRING { Css_types.Value.of_any v }
  ;

