module Value : sig
  type t = string
end

module Declaration : sig
  type t = {property: string; value: Value.t}
end

module Block : sig
  type t = Declaration.t list
end

module Rule : sig
  type t = {
    prelude: string list;
    block: Block.t;
  }
end

module Stylesheet : sig
  type t = Rule.t list
end
