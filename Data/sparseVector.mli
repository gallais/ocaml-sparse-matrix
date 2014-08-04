open Index
open AdditiveGroup
open Ring

module type S = sig
  type zeroFree
  type whoKnows
  type idx
  type ag
  type 'a t

  val is_empty : zeroFree t -> bool
  val safeGet : idx -> zeroFree t -> (ag -> 'b) -> 'b -> 'b
  val getDefault : idx -> zeroFree t -> ag -> ag
  val get        : idx -> zeroFree t -> ag
  val getOpt     : idx -> zeroFree t -> ag option
  val set : idx -> ag -> 'a t -> 'a t
  val fold : (idx -> ag -> 'b -> 'b) -> zeroFree t -> 'b -> 'b
  val mergeWith : (idx -> ag option -> ag option -> ag option) ->
                  'a t -> 'b t -> zeroFree t
  val tabulate : idx -> (idx -> ag) -> zeroFree t
  val mapAll : idx -> (idx -> ag option -> ag) -> ag t -> zeroFree t
  val map    : (idx -> ag -> ag) -> 'a t -> zeroFree t
  val trim   : 'a t -> zeroFree t
  val coerce : zeroFree t -> 'a t
  val equal : (ag -> ag -> bool) -> zeroFree t -> zeroFree t -> bool
  val safeEqual : (ag -> ag -> bool) -> 'a t -> 'b t -> bool
  val zero      : zeroFree t
  val singleton : idx -> ag -> zeroFree t
  val show : string -> (ag -> string) -> idx -> 'a t -> string
  module AG : AdditiveGroup with type t = zeroFree t
end

module Make (I : Index) (AG : AdditiveGroup) :
  S with type idx = I.t and type ag = AG.t
