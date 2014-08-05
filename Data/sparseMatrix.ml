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

type zF = Table.zF
type wK = Table.wK

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
  Table.safeGet i m.table (fun t -> IVector.safeGet j t f dflt) dflt

let getDefault i j m (dflt : R.t) : R.t =
  safeGet i j m (fun x -> x) dflt

let get (i : I.t) (j : I.t) (m : 'a t) : R.t =
  safeGet i j m (fun x -> x) R.zero

let getOpt (i : I.t) (j : I.t) (m : 'a t) : R.t option =
  safeGet i j m some None

let set (i : I.t) (j : I.t) (v : R.t) (m : 'a t) : 'a t =
  let setRow = Table.safeGet i m.table
                (IVector.set j v)
                (IVector.singleton j v) in
  let table = Table.set i setRow m.table in
  { m with table }

let fold (f : I.t -> I.t -> 'a -> 'b -> 'b) (m : zF t) b : 'b =
  Table.fold (fun i -> IVector.fold (f i)) m.table b

let transpose (m : zF t) : zF t =
  let res = { width  = m.height
            ; height = m.width
            ; table  = Table.zero }
  in fold (fun i j -> set j i) m res

let mergeWith f =
  Table.mergeWith (fun i ->
    AGExtIV.liftNonZero2 (IVector.mergeWith (f i)))

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

let makeT (width : I.t) (height : I.t) table : zF t =
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

let zero (width : I.t) (height : I.t) : zF t =
  { width; height; table = Table.zero }

end
