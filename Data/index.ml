module type Index = sig
  include BatInterfaces.OrderedType
  val primrec : (t -> 'a -> 'a) -> 'a -> t -> 'a
  val fromNat : int -> t
end

module IndexInt : Index with type t = int = struct
  type t        = int
  let compare   = compare
  let primrec s =
    let rec aux acc i = if i <= 0 then acc else aux (s i acc) (i - 1)
    in aux
  let fromNat x = assert (0 <= x); x
end

module IndexBig : Index with type t = BatBig_int.t = struct
  type t        = BatBig_int.t
  let compare   = BatBig_int.compare
  let primrec s =
    let rec aux acc i =
      if BatBig_int.compare i BatBig_int.zero <= 0
      then acc
      else aux (s i acc) (BatBig_int.pred i)
    in aux
  let fromNat x =
    assert (0 <= x); BatBig_int.of_int x
end
