open Index
open AdditiveGroup
open Ring

module type S = sig
  type zF (* zero Free! *)
  type wK (* who Knows? *)
  type idx
  type ag
  type 'a t

  val is_empty   : zF t -> bool
  val safeGet    : idx -> zF t -> (ag -> 'b) -> 'b -> 'b
  val getDefault : idx -> zF t -> ag -> ag
  val get        : idx -> zF t -> ag
  val getOpt     : idx -> zF t -> ag option
  val set        : idx -> ag -> 'a t -> 'a t
  val fold       : (idx -> ag -> 'b -> 'b) -> zF t -> 'b -> 'b
  val mergeWith  : (idx -> ag option -> ag option -> ag option) ->
                   'a t -> 'b t -> zF t
  val tabulate   : idx -> (idx -> ag) -> zF t
  val mapAll     : idx -> (idx -> ag option -> ag) -> 'a t -> zF t
  val map        : (idx -> ag -> ag) -> 'a t -> zF t
  val trim       : 'a t -> zF t
  val coerce     : zF t -> 'a t
  val equal      : (ag -> ag -> bool) -> zF t -> zF t -> bool
  val safeEqual  : (ag -> ag -> bool) -> 'a t -> 'b t -> bool
  val constant   : idx -> ag -> zF t
  val zero       : zF t
  val singleton  : idx -> ag -> zF t
  val show       : string -> (ag -> string) -> idx -> 'a t -> string
  module AG      : AdditiveGroup with type t = zF t
end

module type Sext = sig
  include S
  val mult : 'a t -> 'b t -> ag
end

module Make (I : Index) (AG : AdditiveGroup) :
  S with type idx = I.t and type ag = AG.t

module MakeExt (I : Index) (R : Ring) :
  Sext with type idx = I.t and type ag = R.t
