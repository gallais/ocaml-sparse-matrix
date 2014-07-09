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
  let equalZero   = R.equal R.zero
  let fromZeroOpt = function None -> R.zero | Some x -> x
  let isZeroOpt a = if equalZero a then None else Some a
  let liftNonZero op a = isZeroOpt (op (fromZeroOpt a))
  let liftNonZero2 op a b =
    isZeroOpt (op (fromZeroOpt a) (fromZeroOpt b))
  let equalAndNonZero (eq : R.t -> R.t -> bool) a b =
    assert (not (equalZero a) && not (equalZero b));
    eq a b
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
