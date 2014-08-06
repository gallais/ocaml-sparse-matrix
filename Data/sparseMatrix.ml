open Index
open AdditiveGroup
open Ring

open BatOption
open OptionExt


module Make (I : Index) (R : Ring) = struct

module AGExtR  = AGExt (R)
module IVector = SparseVector.MakeExt (I) (R)
module AGExtIV = AGExt (IVector.AG)
module Table   = SparseVector.Make (I) (IVector.AG)

open AGExtR

type zF  = Table.zF
type wK  = Table.wK
type idx = I.t
type ag  = R.t

type 'a t =
  { width  : I.t
  ; height : I.t
  ; table  : 'a Table.t }

let width  m = m.width
let height m = m.height
let table  m = m.table

(** Equality is defined by nesting [Vec]'s parametrized
    equality. It is meant to be called only on 0-free trees
    as the type demonstrates.

    If your inputs may contain [R.zero]s, you *have to* use
    [trim] to clean them up first or [safeEqual] which will
    take care of that (but will discard the trimmed equivalents
    after testing for equality). *)

let equal eq (m : zF t) (n : zF t) : bool =
     m.width  == n.width
  && m.height == n.height
  && Table.equal (IVector.equal eq) m.table n.table

(** Various operation [find]ing the binding at [i] in
    a [Vec] structure. All of them are defined in
    terms of the more general [safeFind].

    [safeFind] alters the returned value using [f] or
    cathes the [Not_found] exception and returns the
    default value [dflt].

    [safeGet] corresponds to 2 nested [safeFind] thus
    allowing us to safely query a matrix. *)

let getRow (i : I.t) (m : 'a t) =
  Table.getDefault i m.table IVector.zero

let getCol (j : I.t) (m : 'a t) =
  Table.fold (fun i row col ->
    IVector.safeGet j row (fun v -> IVector.set i v col) col)
  m.table IVector.zero

let safeGet i j (m : zF t) (f : R.t -> 'b) (dflt : 'b) : 'b =
  if m.height <= i || m.width <= j
  then raise (Invalid_argument "Index out of bound")
  else
    let safeGetValue j row = IVector.safeGet j row f dflt in
    Table.safeGet i m.table (safeGetValue j) dflt

let getDefault i j m (dflt : R.t) : R.t =
  safeGet i j m (fun x -> x) dflt

let get (i : I.t) (j : I.t) (m : 'a t) : R.t =
  safeGet i j m (fun x -> x) R.zero

let getOpt (i : I.t) (j : I.t) (m : 'a t) : R.t option =
  safeGet i j m some None

let set (i : I.t) (j : I.t) (v : R.t) (m : 'a t) : 'a t =
  if m.height <= i || m.width <= j
  then raise (Invalid_argument "Index out of bound")
  else
    let setRow = Table.safeGet i m.table (IVector.set j v)
                 (IVector.singleton j v) in
    let table = Table.set i setRow m.table in
    { m with table }

let fold (f : I.t -> I.t -> 'a -> 'b -> 'b) (m : zF t) b : 'b =
  Table.fold (fun i -> IVector.fold (f i)) m.table b

let transpose (m : zF t) : zF t =
  let res = { height = m.width
            ; width  = m.height
            ; table  = Table.zero }
  in fold (fun i j -> set j i) m res

let mergeWith f m n =
  if (m.width <> n.width || m.height <> n.height)
  then
    let errormsg = "[mergeWith] inputs have distinct geometries"
    in raise (Invalid_argument errormsg)
  else
    let table =
      Table.mergeWith (fun i ->
        AGExtIV.liftNonZero2 (IVector.mergeWith (f i)))
        m.table n.table
    in { m with table }

(** [plus] and [minus] are merging two matrices of the same
    geometry. Together with [opp], they are merely lifting
    the definitions defined in [Table.AG]. *)

let plus (m : 'a t) (n : 'b t) : zF t =
  if (m.width <> n.width || m.height <> n.height) then
  raise (Invalid_argument "[plus] inputs have distinct geometries")
  else { m with table = Table.AG.plus m.table n.table }

let minus (m : 'a t) (n : 'b t) : zF t =
  if (m.width <> n.width || m.height <> n.height) then
  raise (Invalid_argument "[minus] inputs have distinct geometries")
  else { m with table = Table.AG.minus m.table n.table }

let opp (m : 'a t) : zF t =
 { m with table = Table.AG.opp m.table }

let mult (m : zF t) (n : 'a t) : zF t =
  if m.width <> n.height
  then raise (Invalid_argument "[mult] incompatible sizes")
  else
    let n_    = transpose n in
    let table =
      Table.fold (fun i mi_ -> Table.set i
     (Table.fold (fun j n_j -> IVector.set j (IVector.mult mi_ n_j))
        n_.table IVector.zero)) m.table Table.zero
    in { height = m.height; width = n.width; table }

let tabulate height width (f : I.t -> I.t -> R.t) : zF t =
  let table =
    Table.tabulate height
     (fun i -> IVector.tabulate width (f i)) in
  { height; width; table }

(*
(* We assume here that `f` delivers rows of length `width`. *)
let rawTabulateRows (width : I.t) (height : I.t)
  (f : I.t -> R.t Vector.t option) : 'b t =
  let addIthCol i table = optionElim (add i) (f i) table
  in let table = I.primrec addIthCol empty height
  in { width; height; table }

let tabulateCols (width : I.t) (height : I.t)
  (f : I.t -> R.t Vector.t option) : zF t =
  transpose (rawTabulateRows height width f)

let tabulate (width : I.t) (height : I.t)
  (f : I.t -> I.t -> R.t option) : zF t =
  let addIthJth i j = optionElim (add j) (bind (f i j) isZeroOpt)
  in let tabulateRow i =
       isEmptyOpt (I.primrec (addIthJth i) empty width)
  in rawTabulateRows width height tabulateRow
*)

(** [mapAll] is pretty inefficient but you get what you
    ask for: it goes through every single cell!

    [map], on the other hand is more efficient: it only applies
    its input function to non-zero cells.

    [trim] is just one really boring instance of [map] but
    it plays an important role in the definition of [safeEqual]. *)

(*
let mapAll (f : I.t -> I.t -> R.t option -> R.t option)
  (m : 'a t) : zF t =
  tabulate m.width m.height (fun i j -> f i j (getOpt i j m))

*)

let map f (m : 'a t) : zF t =
  let table = Table.map (fun i -> IVector.map (f i)) m.table
  in { m with table }

let makeT (height : I.t) (width : I.t) table : zF t =
  let test i j r = if i < height && j < width then R.zero else r in
  map test { width; height; table }

let trim (m : 'a t) : zF t = map (fun i j x -> x) m

(** [safeEqual] starts by trimming its input so that they
    are safe to use with [equal]. *)

let safeEqual eq (m : 'a t) (n : 'b t) : bool =
  equal eq (trim m) (trim n)

(** Probably quite inefficient [show] and [print] instances.
    They display *every single cell* with no effort whatsoever
    in terms of alignment.
    Their main purpose at the moment is to help debugging. *)

let show (m : 'a t) : string =
  Table.show "\n" (IVector.show " " R.show m.width) m.height m.table

let print (m : 'a t) : unit = print_endline (show m)


(** Some well-known matrices. *)

let diag (size : I.t) (rs : 'a IVector.t) : zF t =
  let alg i el ih =
    if size <= i then ih
    else Table.set i (IVector.singleton i el) ih
  in let table = IVector.fold alg rs Table.zero
  in { height = size; width = size; table }

let id (size : I.t) : zF t = diag size (IVector.constant size R.unit)

let zero (height : I.t) (width : I.t) : zF t =
  { width; height; table = Table.zero }

let singleton h w i j v = set i j v (zero h w)

end

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
