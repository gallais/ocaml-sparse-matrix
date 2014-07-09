open Index
open Ring

open BatOption
open OptionExt

module SparseMatrix (I : Index) (R : Ring) = struct

module RingExtR = RingExt (R)
module Vec  = BatMap.Make (I)
open RingExtR
open Vec

let isEmptyOpt (m : 'a Vec.t) =
  if is_empty m then None else Some m

type table = R.t Vec.t Vec.t
type zeroFree
type unknown

type 'a t =
  { width  : I.t
  ; height : I.t
  ; table  : table }

let width  m = m.width
let height m = m.height
let table  m = m.table

let makeT (width : I.t) (height : I.t) (table : table) : unknown t =
  { width; height; table }

(** Equality is defined by nesting [Vec]'s parametrized
    equality. It is meant to be called only on 0-free trees
    as the type demonstrates.

    If your inputs may contain [R.zero]s, you *have to* use
    [trim] to clean them up first or [safeEqual] which will
    take care of that (but will discard the trimmed equivalents
    after testing for equality). *)

let equal eq (m : zeroFree t) (n : zeroFree t) : bool =
     m.width  == n.width
  && m.height == n.height
  && equal (equal eq) m.table n.table

(** Various operation [find]ing the binding at [i] in
    a [Vec] structure. All of them are defined in
    terms of the more general [safeFind].

    [safeFind] alters the returned value using [f] or
    cathes the [Not_found] exception and returns the
    default value [dflt].

    [safeGet] corresponds to 2 nested [safeFind] thus
    allowing us to safely query a matrix. *)

let safeFind i (m : 'a Vec.t) (f : 'a -> 'b) (dflt : 'b) =
  try f (find i m) with Not_found -> dflt

let findDefault (i : I.t) (m : 'a Vec.t) (dflt : 'a) : 'a =
  safeFind i m (fun x -> x) dflt

let getRow (i : I.t) (m : 'a t) : R.t Vec.t =
  findDefault i m.table empty

let getCol (j : I.t) (m : 'a t) : R.t Vec.t =
  fold (fun i row col -> safeFind j row (fun v -> add i v col) col)
  m.table empty

let safeGet i j (m : 'a t) (f : 'a -> 'b) (dflt : 'b) : 'b =
  safeFind i m.table (fun t -> safeFind j t f dflt) dflt

let get (i : I.t) (j : I.t) (m : 'a t) : R.t =
  safeGet i j m (fun x -> x) R.zero

let getOpt (i : I.t) (j : I.t) (m : 'a t) : R.t option =
  safeGet i j m some None

let setTable (i : I.t) (j : I.t) (v : R.t) (m : table) : table =
  if R.equal R.zero v then m
  else add i (safeFind i m (add j v) (singleton j v)) m

let set (i : I.t) (j : I.t) (v : R.t) (m : 'a t) : 'a t =
  let table = setTable i j v m.table
  in { m with table }

let foldTable (f : I.t -> I.t -> 'a -> 'b -> 'b) (m : table) b : 'b =
  fold (fun i -> fold (fun j -> f i j)) m b

let fold (f : I.t -> I.t -> 'a -> 'b -> 'b) (m : zeroFree t) b : 'b =
  foldTable f m.table b

(** /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
    [transpose] is non involutive in general: it trims
    the tree! Now, if the input is 0-free, then it is
    involutive. *)

let transpose (m : 'a t) : zeroFree t =
  let table = foldTable (fun i j -> setTable j i) m.table empty
  in { width = m.height; height = m.width; table }

(** [plus] is merging two matrices of the same geometry. *)

let plus (m : 'a t) (n : 'b t) : zeroFree t =
  if (m.width <> n.width || m.height <> n.height)
  then raise (Invalid_argument "[plus] inputs of different sizes")
  else
    let table =
      merge (fun _ -> liftOpt2 (fun rowm rown ->
        isEmptyOpt (merge (fun _ -> liftNonZero2 R.plus) rowm rown)))
      m.table n.table
    in { width = m.width; height = m.height; table }

(* We assume here that `f` delivers rows of length `width`. *)
let rawTabulateRows (width : I.t) (height : I.t)
  (f : I.t -> R.t Vec.t option) : 'b t =
  let addIthCol i table = optionElim (add i) (f i) table
  in let table = I.primrec addIthCol empty height
  in { width; height; table }

let tabulateCols (width : I.t) (height : I.t)
  (f : I.t -> R.t Vec.t option) : zeroFree t =
  transpose (rawTabulateRows height width f)

let tabulate (width : I.t) (height : I.t)
  (f : I.t -> I.t -> R.t option) : zeroFree t =
  let addIthJth i j = optionElim (add j) (bind (f i j) isZeroOpt)
  in let tabulateRow i =
       isEmptyOpt (I.primrec (addIthJth i) empty width)
  in rawTabulateRows width height tabulateRow

(** [mapAll] is pretty inefficient but you get what you
    ask for: it goes through every single cell!

    [mapRow] and [map], on the other hand are more efficient:
    they only apply their input function to non-zero cells.
    [map] proceeds row by row which should be more efficient
    than simply using [foldTable] together with [setTable].

    [trim] is just one really boring instance of [map] but
    it plays an important role in the definition of [safeEqual]. *)

let mapAll (f : I.t -> I.t -> R.t option -> R.t option)
  (m : 'a t) : zeroFree t =
  tabulate m.width m.height (fun i j -> f i j (getOpt i j m))

let mapRow (f : I.t -> 'a -> 'a option) (row : 'a Vec.t) =
  Vec.fold (fun j elt -> optionElim (add j) (f j elt)) row empty

let rawMap (f : I.t -> I.t -> R.t -> R.t option)
  (m : 'a t) : 'b t =
  let width  = m.width  in
  let height = m.height in
  let table  = Vec.fold (fun i row -> add i (mapRow (f i) row)) m.table empty
  in { width; height; table }

let map (f : I.t -> I.t -> R.t -> R.t option)
    (m : 'a t) : zeroFree t =
    rawMap (fun i j a ->
      bind (isZeroOpt a) (fun a -> bind (f i j a) isZeroOpt))
    m

let trim (m : 'a t) : zeroFree t = map (fun i j -> some) m
let coerce m = (m :> 'a t)

(** [safeEqual] starts by trimming its input so that they
    are safe to use with [equal]. *)

let safeEqual eq (m : 'a t) (n : 'b t) : bool =
  let m = trim m in
  let n = trim n in
  equal eq m n

(** Probably quite inefficient [show] and [print] instances.
    They display *every single cell* with no effort whatsoever
    in terms of alignment.
    Their main purpose at the moment is to help debugging. *)

let showRow (row : R.t Vec.t) (width : I.t) : string =
  String.concat " "
    (I.primrec (fun j s -> R.show (findDefault j row R.zero) :: s)
    [] width)

let show (m : 'a t) : string =
  String.concat "\n"
    (I.primrec (fun i ss ->
      showRow (findDefault i m.table empty) m.width :: ss)
    [] m.height)

let print (m : 'a t) : unit = print_endline (show m)

(** Some well-known matrices. *)

let id (size : I.t) : zeroFree t =
  rawTabulateRows size size (fun i -> Some (singleton i R.unit))

let zero (width : I.t) (height : I.t) : zeroFree t =
  { width; height; table = empty }

end
