module type S = sig
  type zF
  type wK

  type idx
  type ag
  type 'a t

  val width      : 'a t -> idx
  val height     : 'a t -> idx
  val equal      : (ag -> ag -> bool) -> zF t -> zF t -> bool
  val safeGet    : idx -> idx -> zF t -> (ag -> 'b) -> 'b -> 'b
  val getDefault : idx -> idx -> zF t -> ag -> ag
  val get        : idx -> idx -> zF t -> ag
  val getOpt     : idx -> idx -> zF t -> ag option
  val set        : idx -> idx -> ag -> zF t -> zF t
  val fold       : (idx -> idx -> ag -> 'b -> 'b) -> zF t -> 'b -> 'b
  val transpose  : zF t -> zF t
  val mergeWith  :
    (idx -> idx -> ag option -> ag option -> ag option) ->
    'a t -> 'b t -> zF t
  val plus       : zF t -> zF t -> zF t
  val minus      : zF t -> zF t -> zF t
  val opp        : zF t -> zF t
  val mult       : zF t -> zF t -> zF t
  val tabulate   : idx -> idx -> (idx -> idx -> ag) -> zF t
  val map        : (idx -> idx -> ag -> ag) -> 'a t -> zF t
  val trim       : 'a t -> zF t
  val safeEqual  : (ag -> ag -> bool) -> 'a t -> 'b t -> bool
  val show       : 'a t -> string
  val print      : 'a t -> unit
  val singleton  : idx -> idx -> idx -> idx -> ag -> zF t
  val id         : idx -> zF t
  val zero       : idx -> idx -> zF t
end

open Index
open Ring

module Make (I : Index) (R : Ring)
  : S with type idx = I.t and type ag = R.t
