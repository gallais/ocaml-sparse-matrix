open Index
open Ring

module SparseMatrix (I : Index) (R : Ring) : sig

module Vec : sig type 'a t end

(** Phantom types *)
type zeroFree
type whoKnows

(** Matrix type *)
type 'a t
val width  : 'a t -> I.t
val height : 'a t -> I.t
val table  : 'a t -> R.t Vec.t Vec.t
val makeT  : I.t -> I.t -> R.t Vec.t Vec.t -> whoKnows t

(** Safe accessors all implemented by instantiating the
    corresponding [safeXxxx] version which catches
    [Not_found] exceptions. *)
val safeFind    : I.t -> 'a Vec.t -> ('a -> 'b) -> 'b -> 'b
val findDefault : I.t -> 'a Vec.t -> 'a -> 'a

val getRow : I.t -> 'a t -> R.t Vec.t
val getCol : I.t -> 'a t -> R.t Vec.t

val safeGet : I.t -> I.t -> R.t t -> (R.t -> 'b) -> 'b -> 'b
val get     : I.t -> I.t -> R.t t -> R.t
val getOpt  : I.t -> I.t -> R.t t -> R.t option

(** Safe setter preserving the invariant. *)
val set : I.t -> I.t -> R.t -> 'a t -> 'a t

(***)
val fold : (I.t -> I.t -> R.t -> 'b -> 'b) -> zeroFree t -> 'b -> 'b

(** Typical operations on matrices. Notice how both of these
    operation make sure the invariant is respected. *)
val transpose : 'a t -> zeroFree t
val plus : 'a t -> 'b t -> zeroFree t

val tabulateCols : I.t -> I.t -> (I.t -> R.t Vec.t option) ->
                   zeroFree t
val tabulate : I.t -> I.t -> (I.t -> I.t -> R.t option) -> zeroFree t
val mapAll   : (I.t -> I.t -> R.t option -> R.t option) ->
               R.t t -> zeroFree t

val map    : (I.t -> I.t -> R.t -> R.t option) -> 'a t -> zeroFree t
val trim   : 'a t -> zeroFree t
val coerce : zeroFree t -> 'a t

(** Equality testing.
    In the case where matrices are not statically known to
    be zeroFree, [safeEqual] starts by [trim]ming them. *)
val equal : (R.t -> R.t -> bool) -> zeroFree t -> zeroFree t -> bool
val safeEqual : (R.t -> R.t -> bool) -> 'a t -> 'b t -> bool

val show  : 'a t -> string
val print : 'a t -> unit

val id : I.t -> zeroFree t
val zero : I.t -> I.t -> zeroFree t
end
