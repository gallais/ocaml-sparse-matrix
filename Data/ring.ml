open AdditiveGroup

module type Ring = sig
  include AdditiveGroup
  val unit    : t
  val mult    : t -> t -> t
  val show    : t -> string
end

module RingInt : Ring with type t = int = struct
  include AGInt
  let unit = 1
  let mult a b = a * b
  let show      = Printf.sprintf "%d"
end

module RingFloat : Ring with type t = float = struct
  include AGFloat
  let unit     = 1.0
  let mult a b = a *. b
  let show      = Printf.sprintf "%.5f"
end
