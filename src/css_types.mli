module Value :
sig
  type t

  val of_any: string -> t

  val to_string: t -> string

end

module Declaration :
sig
  type t = {
    property : string;
    value : Value.t;
  }

end

module Block :
sig
  type t = Declaration.t list

end

