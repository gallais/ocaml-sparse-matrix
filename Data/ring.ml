module type Ring = sig
  type t
  val  zero    : t
  val  unit    : t
  val  plus    : t -> t -> t
  val  mult    : t -> t -> t
  val  opp     : t -> t
  val  equal   : t -> t -> bool
  val  show    : t -> string
end

module RingExt (R : Ring) = struct
  let fromOption = function None -> R.zero | Some x -> x
  let toOption a = if R.equal a R.zero then None else Some a
  let liftNonZero op a b =
    toOption (op (fromOption a) (fromOption b))
end

module RingInt : Ring with type t = int = struct
  type t = int
  let zero = 0
  let unit = 1
  let plus = (+)
  let mult a b = a * b
  let opp a = - a
  let equal a b = a == b
  let show = Printf.sprintf "%d"
end

module RingFloat : Ring with type t = float = struct
  type t = float
  let zero = 0.
  let unit = 1.
  let plus = (+.)
  let mult a b = a *. b
  let opp a = -. a
  let equal a b = a == b
  let show = Printf.sprintf "%.5f"
end
