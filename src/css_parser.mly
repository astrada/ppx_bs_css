%token <string> STRING
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMI_COLON
%token COLON
%token EOF

%start <Css_types.Stylesheet.t> stylesheet

%%

stylesheet:
  rs = list(rule); EOF { rs }
  ;

rule:
  xs = prelude; b = block { { Css_types.Rule.prelude = xs; Css_types.Rule.block = b; } }
  ;

prelude:
  xs = list(STRING) { xs }
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
  p = STRING; COLON; v = value { { Css_types.Declaration.property = p; value = v } }
  ;

value:
  v = STRING { v }
  ;

