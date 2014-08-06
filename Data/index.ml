module type Index = sig
  include BatInterfaces.OrderedType
  val primrec : (t -> 'a -> 'a) -> 'a -> t -> 'a
  val pred    : t -> t
  val fromNat : int -> t
end

module IndexInt : Index with type t = int = struct
  type t        = int
  let compare   = compare
  let primrec s =
    let rec aux acc i = if i <= 0 then acc else aux (s i acc) (i - 1)
    in aux
  let pred      = function 0 -> 0 | x -> x - 1
  let fromNat x = assert (0 <= x); x
end

module IndexBig : Index with type t = BatBig_int.t = struct
  open BatBig_int
  type t        = BatBig_int.t
  let compare   = compare
  let primrec s =
    let rec aux acc i =
      if compare i zero <= 0
      then acc
      else aux (s i acc) (pred i)
    in aux
  let pred    x = if compare x zero <= 0 then zero else pred x
  let fromNat x =
    assert (0 <= x); of_int x
end
