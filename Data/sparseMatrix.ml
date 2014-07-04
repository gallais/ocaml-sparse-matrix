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

let equal (m : t) (n : t) : bool =
     m.width  == n.width
  && m.height == n.height
  && equal (equal (==)) m.table n.table

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

let getOpt (i : I.t) (j : I.t) (m : t) : R.t option =
  try Some (find j (find i m.table)) with Not_found -> None

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

let print (m : t) : unit = print_endline (show m)

let plus (m : t) (n : t) : t =
  if (m.width <> n.width || m.height <> n.height)
  then raise (Invalid_argument "[plus] inputs of different sizes")
  else
    let table = merge (fun _ (mrowm : R.t BatMapI.t option) mrown ->
      match mrowm, mrown with
      | None , _ -> mrown
      | _ , None -> mrowm
      | Some rowm , Some rown ->
        let row = merge (fun _ -> liftNonZero R.plus) rowm rown
        in if is_empty row then None else Some row) m.table n.table
    in { width = m.width; height = m.height; table }

(* We assume here that `f` delivers rows of length `width`. *)
let tabulateRows (width : I.t) (height : I.t)
  (f : I.t -> R.t BatMapI.t option) : t =
  let addIthCol i table =
    let fi = f i in
    match fi with None -> table | Some fi -> add i fi table
  in let table = I.primrec addIthCol empty height
  in { width; height; table }

let tabulateCols (width : I.t) (height : I.t)
  (f : I.t -> R.t BatMapI.t option) : t =
  transpose (tabulateRows height width f)

let tabulate (width : I.t) (height : I.t)
  (f : I.t -> I.t -> R.t option) : t =
  let addIthJth i j acc =
     let fij = f i j in
     match fij with None -> acc | Some v -> add j v acc
  in let tabulateRow i =
       let row = I.primrec (addIthJth i) empty width in
       if is_empty row then None else Some row
  in tabulateRows width height tabulateRow

let mapAll (f : R.t option -> R.t option) (m : t) : t =
  tabulate m.width m.height (fun i j -> f (getOpt i j m))

let map (f : R.t -> R.t option) (m : t) : t =
  let width  = m.width  in
  let height = m.height in
  let table  =
    fold (fun i row -> add i (fold (fun j elt row ->
    match f elt with None -> row | Some v -> add j v row) row empty)) m.table empty
  in { width; height; table }

let trim (m : t) : t = map (fun x -> if R.equal x R.zero then None else Some x) m

(* If any of [m] or [n] contains [R.zero]s than they can be declared
   different when, morally, they are equal. [safeEqual] trims both of
   its inputs before testing for equality. *)
let safeEqual (m : t) (n : t) : bool =
  let m = trim m in
  let n = trim n in
  equal m n

let id (size : I.t) : t =
  tabulateRows size size (fun i -> Some (singleton i R.unit))

let zero (width : I.t) (height : I.t) : t =
  { width; height; table = empty }

end

