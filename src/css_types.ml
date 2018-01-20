module Value =
struct
  type t = string

  let of_any x = x

  let to_string x = x

end

module Declaration =
struct
  type t = {
    property : string;
    value : Value.t;
  }

end

module Block =
struct
  type t = Declaration.t list

end

