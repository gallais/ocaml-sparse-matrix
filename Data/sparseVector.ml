open Index
open AdditiveGroup

module Make (I : Index) (AG : AdditiveGroup) = struct

module Vector = BatMap.Make (I)
module AGExt  = AGExt (AG)
open AGExt

open OptionExt

type zeroFree
type whoKnows

type idx  = I.t
type ag   = AG.t
type 'a t = AG.t Vector.t


let is_empty (v : zeroFree t) = Vector.is_empty v

let safeGet i (m : 'a t) (f : 'a -> 'b) (dflt : 'b) =
  try f (Vector.find i m) with Not_found -> dflt

let getDefault i m = safeGet i m (fun x -> x)
let get        i m = safeGet i m (fun x -> x) AG.zero
let getOpt     i m = safeGet i m some None

let fold (c : I.t -> AG.t -> 'b -> 'b) (v : zeroFree t) (b : 'b) : 'b =
  Vector.fold c v b

let set (i : I.t) (v : AG.t) (m : 'a t) : 'a t =
  if AG.equal AG.zero v then m
  else Vector.add i v m

let mergeWith f (v : 'a t) (w : 'b t) : zeroFree t =
  let f' i vi wi =
    checkOptIfZero (f i (checkOptIfZero vi) (checkOptIfZero wi)) in
  Vector.merge f' v w

let tabulate (m : I.t) (f : I.t -> AG.t) : zeroFree t =
  I.primrec (fun i -> set i (f i)) Vector.empty m

let mapAll (m : I.t) (f : I.t -> AG.t option -> AG.t) (v : AG.t t) =
  tabulate m (fun i -> f i (getOpt i v))

let map (f : I.t -> AG.t -> AG.t) (v : 'a t) : zeroFree t =
  Vector.fold (fun i v -> set i (f i v)) v Vector.empty

let trim     = map (fun i r -> r)
let coerce x = x

let show inter (show : AG.t -> string) (size : I.t) (v : 'a t) =
  String.concat inter
    (I.primrec (fun i ss -> show (get i v) :: ss) [] size)

let equal eq (v : zeroFree t) (w : zeroFree t) : bool =
  Vector.equal eq v w

let safeEqual eq (v : 'a t) (w : 'b t) : bool =
  equal eq (trim v) (trim w)

let zero          = Vector.empty
let singleton i r = set i r zero


module AG : AdditiveGroup with type t = zeroFree t = struct
  type t    = AG.t Vector.t
  let zero  = zero
  let plus  = mergeWith (fun _ -> liftNonZero2 AG.plus)
  let opp   = map (fun _ -> AG.opp)
  let minus = mergeWith (fun _ -> liftNonZero2 AG.minus)
  let equal = equal (AG.equal)
end

end

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
