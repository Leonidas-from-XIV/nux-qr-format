module Safe = struct
  type t =
    [ `Bool of bool
    | `String of string
    | `Int of int
    | `List of t list
    | `Assoc of (string * t) list
    | `Float of float
    | `Intlit of string
    | `Null
    | `Tuple of t list
    | `Variant of string * t option ]
end
