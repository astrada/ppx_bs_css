module Value = struct
  type t = string
end

module Declaration = struct
  type t = {property: string; value: Value.t}
end

module Block = struct
  type t = Declaration.t list
end

module Rule = struct
  type t = {
    prelude: string list;
    block: Block.t;
  }
end

module Stylesheet = struct
  type t = Rule.t list
end
