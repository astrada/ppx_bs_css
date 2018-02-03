%token EOF
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token COLON
%token SEMI_COLON
%token PERCENTAGE
%token IMPORTANT
%token <string> IDENT
%token <string> STRING
%token <string> URI
%token <string> OPERATOR
%token <string> DELIM
%token <string> AT_RULE
%token <string> FUNCTION
%token <string> HASH
%token <string> NUMBER
%token <string> UNICODE_RANGE
%token <string * string> DIMENSION

%start <Css_types.Stylesheet.t> stylesheet
%start <Css_types.Declaration_list.t> declaration_list

%%

stylesheet:
  rs = list(rule); EOF { rs }
  ;

declaration_list:
  ds = declarations; EOF { ds }
  ;

rule:
  | r = at_rule { Css_types.Rule.At_rule r }
  | r = style_rule { Css_types.Rule.Style_rule r }
  ;

at_rule:
  | name = AT_RULE; xs = prelude; SEMI_COLON { { Css_types.At_rule.name = name; prelude = xs; block = None; } }
  | name = AT_RULE; xs = prelude; b = brace_block { { Css_types.At_rule.name = name; prelude = xs; block = Some (Css_types.Component_value.Brace_block b); } }
  ;

style_rule:
  xs = prelude; LEFT_BRACE; ds = declarations; RIGHT_BRACE { { Css_types.Style_rule.prelude = xs; block = ds; } }
  ;

prelude:
  xs = list(component_value_without_brace_block) { xs }
  ;

declaration_or_at_rule:
  | d = declaration { Css_types.Declaration_list.Declaration d }
  | r = at_rule { Css_types.Declaration_list.At_rule r }
  ;

declarations_without_ending_semi_colon:
  | d = declaration_or_at_rule { [d] }
  | ds = declarations_without_ending_semi_colon; SEMI_COLON; d = declaration_or_at_rule { d :: ds }
  ;

declarations:
  | ds = declarations_without_ending_semi_colon { List.rev ds }
  | ds = declarations_without_ending_semi_colon; SEMI_COLON { List.rev ds }
  ;

declaration:
  n = IDENT; COLON; v = list(component_value); i = boption(IMPORTANT) { { Css_types.Declaration.name = n; value = v; important = i } }
  ;

brace_block:
  LEFT_BRACE; xs = list(component_value); RIGHT_BRACE { xs }
  ;

paren_block:
  LEFT_PAREN; xs = list(component_value); RIGHT_PAREN { xs }
  ;

bracket_block:
  LEFT_BRACKET; xs = list(component_value); RIGHT_BRACKET { xs }
  ;

component_value:
  | b = brace_block { Css_types.Component_value.Brace_block b }
  | c = component_value_without_brace_block { c }
  ;

component_value_without_brace_block:
  | b = paren_block { Css_types.Component_value.Paren_block b }
  | b = bracket_block { Css_types.Component_value.Bracket_block b }
  | n = NUMBER; PERCENTAGE { Css_types.Component_value.Percentage n }
  | i = IDENT { Css_types.Component_value.Ident i }
  | s = STRING { Css_types.Component_value.String s }
  | u = URI { Css_types.Component_value.Uri u }
  | o = OPERATOR { Css_types.Component_value.Operator o }
  | d = DELIM { Css_types.Component_value.Delim d }
  | f = FUNCTION; xs = list(component_value); RIGHT_PAREN { Css_types.Component_value.Function (f, xs) }
  | h = HASH { Css_types.Component_value.Hash h }
  | n = NUMBER { Css_types.Component_value.Number n }
  | r = UNICODE_RANGE { Css_types.Component_value.Unicode_range r }
  | d = DIMENSION { Css_types.Component_value.Dimension d }
  ;
