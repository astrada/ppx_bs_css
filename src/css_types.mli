module rec Component_value : sig
  type t =
    | Brace_block of t list
    | Paren_block of t list
    | Bracket_block of t list
    | Percentage of string
    | Ident of string
    | String of string
    | Uri of string
    | Operator of string
    | Delim of string
    | At_rule of At_rule.t
    | Function of (string * t list)
    | Hash of string
    | Number of string
    | Unicode_range of string
    | Dimension of (string * string)
end

and At_rule : sig
  type t =
    {name: string; prelude: Component_value.t list; block: Component_value.t option}
end

module Declaration : sig
  type t = {name: string; value: Component_value.t list; important: bool}
end

module Declaration_list : sig
  type kind =
  | Declaration of Declaration.t
  | At_rule of At_rule.t
  type t = kind list
end

module Style_rule : sig
  type t = {prelude: Component_value.t list; block: Declaration_list.t}
end

module Rule : sig
  type t =
  | Style_rule of Style_rule.t
  | At_rule of At_rule.t
end

module Stylesheet : sig
  type t = Rule.t list
end