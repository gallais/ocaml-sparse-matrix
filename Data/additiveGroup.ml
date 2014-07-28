module type AdditiveGroup = sig
  type t
  val  zero    : t
  val  plus    : t -> t -> t
  val  minus   : t -> t -> t
  val  opp     : t -> t
  val  equal   : t -> t -> bool
end

module AGExt (AG : AdditiveGroup) = struct
  let equalZero   = AG.equal AG.zero
  let fromZeroOpt = function None -> AG.zero | Some x -> x
  let isZeroOpt a = if equalZero a then None else Some a
  let liftNonZero op a = isZeroOpt (op (fromZeroOpt a))
  let liftNonZero2 op a b =
    isZeroOpt (op (fromZeroOpt a) (fromZeroOpt b))
  let checkOptIfZero = liftNonZero (fun x -> x)
  let equalAndNonZero (eq : AG.t -> AG.t -> bool) a b =
    assert (not (equalZero a) && not (equalZero b));
    eq a b
end

module AGInt : AdditiveGroup with type t = int = struct
  type t        = int
  let zero      = 0
  let plus      = (+)
  let minus     = (-)
  let opp a     = - a
  let equal a b = a == b
end

module AGFloat : AdditiveGroup with type t = float = struct
  type t        = float
  let zero      = 0.
  let plus      = (+.)
  let minus     = (-.)
  let opp a     = -. a
  let equal a b = a == b
end
