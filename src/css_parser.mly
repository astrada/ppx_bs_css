%token EOF
%token SEMI_COLON
%token RIGHT_BRACE
%token LEFT_BRACE
%token COLON
%token RIGHT_PAREN
%token PERCENTAGE
%token <string> OPERATOR
%token <string> STRING
%token <string> URI
%token IMPORTANT
%token <string> AT_RULE
%token <string> UNICODE_RANGE
%token <string> FUNCTION
%token <string> IDENT
%token <string> HASH
%token <string> NUMBER
%token <string * string> DIMENSION
%token <string> DELIM

%start <Css_types.Stylesheet.t> stylesheet

%%

stylesheet:
  rs = list(rule); EOF { rs }
  ;

rule:
  xs = prelude; b = block { { Css_types.Rule.prelude = xs; Css_types.Rule.block = b; } }
  ;

prelude:
  xs = list(IDENT) { xs }
  ;

block:
  | LEFT_BRACE; RIGHT_BRACE { [] }
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
  p = IDENT; COLON; v = value { { Css_types.Declaration.property = p; value = v } }
  ;

value:
  v = IDENT { v }
  ;

