open Index
open Ring
open OptionExt

module SparseMatrix (I : Index) (R : Ring) = struct

module RingExtR = RingExt (R)
module BatMapI  = BatMap.Make (I)
open RingExtR
open BatMapI

let isEmptyOpt (m : 'a BatMapI.t) =
  if is_empty m then None else Some m

type table = R.t BatMapI.t BatMapI.t

type t =
  { width  : I.t
  ; height : I.t
  ; table  : table }

(** Equality is defined by nesting [BatMapI]'s parametrized
    equality. It is meant to be called only on 0-free trees
    and will thus raise an exception if it comes across any
    R.zero (failed assert).

    /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
    [equal] can still silently fail if called on non 0-free
    matrices (e.g. [equal [[0]] []] will return [false]!

    If your inputs may contain R.zeros, you *should* use
    [trim] first or [safeEqual]. *)

let equal eq (m : t) (n : t) : bool =
     m.width  == n.width
  && m.height == n.height
  && equal (equal (equalAndNonZero eq)) m.table n.table

(** Various operation [find]ing the binding at [i] in
    a [BatMapI] structure. All of them are defined in
    terms of the more general [safeFind].

    [safeFind] alters the returned value using [f] or
    cathes the [Not_found] exception and returns the
    default value [dflt].

    [safeGet] corresponds to 2 nested [safeFind] thus
    allowing us to safely query a matrix. *)

let safeFind i (m : 'a BatMapI.t) (f : 'a -> 'b) (dflt : 'b) =
  try f (find i m) with Not_found -> dflt

let findDefault (i : I.t) (m : 'a BatMapI.t) (dflt : 'a) : 'a =
  safeFind i m (fun x -> x) dflt

let getRow (i : I.t) (m : t) : R.t BatMapI.t =
  findDefault i m.table empty

let getCol (j : I.t) (m : t) : R.t BatMapI.t =
  fold (fun i row col -> safeFind j row (fun v -> add i v col) col)
  m.table empty

let safeGet i j m (f : 'a -> 'b) (dflt : 'b) : 'b =
  safeFind i m.table (fun t -> safeFind j t f dflt) dflt

let get (i : I.t) (j : I.t) (m : t) : R.t =
  safeGet i j m (fun x -> x) R.zero

let getOpt (i : I.t) (j : I.t) (m : t) : R.t option =
  safeGet i j m some None

let setTable (i : I.t) (j : I.t) (v : R.t) (m : table) : table =
  if R.equal R.zero v then m
  else add i (safeFind i m (add j v) (singleton j v)) m

let set (i : I.t) (j : I.t) (v : R.t) (m : t) : t =
  let table = setTable i j v m.table
  in { m with table }

let foldTable (f : I.t -> I.t -> 'a -> 'b -> 'b) m b : 'b =
  fold (fun i -> fold (fun j -> f i j)) m b

(** /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
    [transpose] is non involutive in general: it trims
    the tree! Now, if the input is 0-free, then it is
    involutive. *)

let transpose (m : t) : t =
  let table = foldTable (fun i j -> setTable j i) m.table empty
  in { width = m.height; height = m.width; table }

(** [plus] is merging two matrices of the same geometry. *)

let plus (m : t) (n : t) : t =
  if (m.width <> n.width || m.height <> n.height)
  then raise (Invalid_argument "[plus] inputs of different sizes")
  else
    let table =
      merge (fun _ -> liftOpt2 (fun rowm rown ->
        isEmptyOpt (merge (fun _ -> liftNonZero R.plus) rowm rown)))
      m.table n.table
    in { width = m.width; height = m.height; table }

(* We assume here that `f` delivers rows of length `width`. *)
let tabulateRows (width : I.t) (height : I.t)
  (f : I.t -> R.t BatMapI.t option) : t =
  let addIthCol i table = optionElim (add i) (f i) table
  in let table = I.primrec addIthCol empty height
  in { width; height; table }

let tabulateCols (width : I.t) (height : I.t)
  (f : I.t -> R.t BatMapI.t option) : t =
  transpose (tabulateRows height width f)

let tabulate (width : I.t) (height : I.t)
  (f : I.t -> I.t -> R.t option) : t =
  let addIthJth i j = optionElim (add j) (f i j)
  in let tabulateRow i =
       isEmptyOpt (I.primrec (addIthJth i) empty width)
  in tabulateRows width height tabulateRow

(** [mapAll] is pretty inefficient but you get what you
    ask for: it goes through every single cell!

    [mapRow] and [map], on the other hand are more efficient:
    they only apply their input function to non-zero cells.
    [map] proceeds row by row which should be more efficient
    than simply using [foldTable] together with [setTable].

    [trim] is just one really boring instance of [map] but
    it plays an important role in the definition of [safeEqual]. *)

let mapAll (f : R.t option -> R.t option) (m : t) : t =
  tabulate m.width m.height (fun i j -> f (getOpt i j m))

let mapRow (f : 'a -> 'a option) (row : 'a BatMapI.t) =
  fold (fun j elt -> optionElim (add j) (f elt)) row empty

let map (f : R.t -> R.t option) (m : t) : t =
  let width  = m.width  in
  let height = m.height in
  let table  = fold (fun i row -> add i (mapRow f row)) m.table empty
  in { width; height; table }

let trim (m : t) : t = map isZeroOpt m

(** If any of [m] or [n] contains [R.zero]s than they can be
    declared different when, morally, they are equal.
    [safeEqual] trims both of its inputs before testing for
    equality thus avoiding this sort of issues. If you've
    built your tree in a 0-avoiding way, then you should be
    able to safely use [equal]. *)

let safeEqual eq (m : t) (n : t) : bool =
  let m = trim m in
  let n = trim n in
  equal eq m n

(** Probably quite inefficient [show] and [print] instances.
    They display *every single cell* with no effort whatsoever
    in terms of alignment.
    Their main purpose at the moment is to help debugging. *)

let showRow (row : R.t BatMapI.t) (width : I.t) : string =
  String.concat " "
    (I.primrec (fun j s -> R.show (findDefault j row R.zero) :: s)
    [] width)

let show (m : t) : string =
  String.concat "\n"
    (I.primrec (fun i ss ->
      showRow (findDefault i m.table empty) m.width :: ss)
    [] m.height)

let print (m : t) : unit = print_endline (show m)

(** Some well-known matrices. *)

let id (size : I.t) : t =
  tabulateRows size size (fun i -> Some (singleton i R.unit))

let zero (width : I.t) (height : I.t) : t =
  { width; height; table = empty }

end

