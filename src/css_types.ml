type 'a with_loc = 'a * Location.t

module rec Component_value : sig
  type t =
    | Brace_block of t with_loc list
    | Paren_block of t with_loc list
    | Bracket_block of t with_loc list
    | Percentage of string
    | Ident of string
    | String of string
    | Uri of string
    | Operator of string
    | Delim of string
    | At_rule of At_rule.t
    | Function of string * t with_loc list
    | Hash of string
    | Number of string
    | Unicode_range of string
    | Dimension of (string * string)
end =
  Component_value

and At_rule : sig
  type t =
    { name: string with_loc;
      prelude: Component_value.t with_loc list with_loc;
      block: Component_value.t with_loc option;
      loc: Location.t;
    }
end =
  At_rule

module Declaration = struct
  type t =
    { name: string with_loc;
      value: Component_value.t with_loc list with_loc;
      important: bool with_loc;
      loc: Location.t;
    }
end

module Declaration_list = struct
  type kind =
    | Declaration of Declaration.t
    | At_rule of At_rule.t
  type t = kind list with_loc
end

module Style_rule = struct
  type t =
    { prelude: Component_value.t with_loc list with_loc;
      block: Declaration_list.t;
      loc: Location.t;
    }
end

module Rule = struct
  type t =
    | Style_rule of Style_rule.t
    | At_rule of At_rule.t
end

module Stylesheet = struct
  type t = Rule.t list with_loc
end
