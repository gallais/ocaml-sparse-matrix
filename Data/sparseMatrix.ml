open Index
open Ring

module SparseMatrix (I : Index) (R : Ring) = struct

module RingExtR = RingExt (R)
module BatMapI  = BatMap.Make (I)
open RingExtR
open BatMapI

type table = R.t BatMapI.t BatMapI.t

type t =
  { width  : I.t
  ; height : I.t
  ; table  : table }

let findDefault (i : I.t) (m : 'a BatMapI.t) (dflt : 'a) : 'a =
  try find i m with Not_found -> dflt

let getRow (i : I.t) (m : t) : R.t BatMapI.t =
  try find i m.table with Not_found -> empty

let getCol (j : I.t) (m : t) : R.t BatMapI.t =
  fold (fun i row col ->
    try add i (find j row) col with Not_found -> col)
  m.table empty

let get (i : I.t) (j : I.t) (m : t) : R.t =
  try find j (find i m.table) with Not_found -> R.zero

let setTable (i : I.t) (j : I.t) (v : R.t) (m : table) : table =
  if R.equal R.zero v then m
  else
    let row = findDefault i m empty in
    add i (add j v row) m

let set (i : I.t) (j : I.t) (v : R.t) (m : t) : t =
  let table = setTable i j v m.table
  in { m with table }

let transpose (m : t) : t =
  let table = fold (fun i -> fold (fun j -> setTable j i))
              m.table empty
  in { width = m.height; height = m.width; table }

let showRow (row : R.t BatMapI.t) (width : I.t) : string =
  String.concat " "
    (I.primrec (fun j s ->
      let el = findDefault j row R.zero in R.show el :: s)
    [] width)

let show (m : t) : string =
  String.concat "\n"
    (I.primrec (fun i ss ->
      let row = findDefault i m.table empty in
      showRow row m.width :: ss) [] m.height)

let plus (m : t) (n : t) : t =
  if (m.width <> n.width || m.height <> n.height)
  then raise (Invalid_argument "[plus] inputs of different sizes")
  else
    let table =
      fold (fun i rowm table ->
        modify_opt i (fun mrown ->
          match mrown with
          | None      -> Some rowm
          | Some rown ->
        let row' =
          fold (fun j eltm rown ->
            modify_opt j (liftNonZero R.plus (Some eltm)) rown)
          rowm rown
        in if is_empty row' then None else Some row') table)
      m.table n.table
  in { width = m.width; height = m.height; table }

(* We assume here that `f` delivers rows of length `width`. *)
let tabulateRows (width : I.t) (height : I.t)
  (f : I.t -> R.t BatMapI.t) : t =
  let addIthCol i = add i (f i) in
  let table = I.primrec addIthCol empty height in
  { width; height; table }

let tabulateCols (width : I.t) (height : I.t)
  (f : I.t -> R.t BatMapI.t) : t =
  transpose (tabulateRows height width f)

let tabulate (width : I.t) (height : I.t)
  (f : I.t -> I.t -> R.t) : t =
  let addIthJth i j acc =
     let v = f i j in
     if R.equal v R.zero then acc else add j v acc
  in let tabulateRow i = I.primrec (addIthJth i) empty width
  in tabulateRows width height tabulateRow

let id (size : I.t) : t =
  tabulateRows size size (fun i -> singleton i R.unit)

let zero (width : I.t) (height : I.t) : t =
  { width; height; table = empty }

end

