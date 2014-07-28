open Index
open AdditiveGroup

module Make (I : Index) (AG : AdditiveGroup) : sig

type zeroFree
type whoKnows

type 'a t

val is_empty : zeroFree t -> bool

val safeGet : I.t -> zeroFree t -> (AG.t -> 'b) -> 'b -> 'b

val getDefault : I.t -> zeroFree t -> AG.t -> AG.t
val get        : I.t -> zeroFree t -> AG.t
val getOpt     : I.t -> zeroFree t -> AG.t option

val set : I.t -> AG.t -> 'a t -> 'a t

val fold : (I.t -> AG.t -> 'b -> 'b) -> zeroFree t -> 'b -> 'b

val mergeWith : (I.t -> AG.t option -> AG.t option -> AG.t option) ->
                'a t -> 'b t -> zeroFree t

val tabulate : I.t -> (I.t -> AG.t) -> zeroFree t

val mapAll : I.t -> (I.t -> AG.t option -> AG.t) -> AG.t t -> zeroFree t

val map    : (I.t -> AG.t -> AG.t) -> 'a t -> zeroFree t
val trim   : 'a t -> zeroFree t
val coerce : zeroFree t -> 'a t

val equal : (AG.t -> AG.t -> bool) -> zeroFree t -> zeroFree t -> bool
val safeEqual : (AG.t -> AG.t -> bool) -> 'a t -> 'b t -> bool

val zero      : zeroFree t
val singleton : I.t -> AG.t -> zeroFree t

val show : string -> (AG.t -> string) -> I.t -> 'a t -> string

module AG : AdditiveGroup with type t = zeroFree t
end
