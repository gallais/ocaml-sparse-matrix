open Index
open Ring
open SparseMatrix
open Testable

module TestSM (I : Index) (R : Ring)
  (T : Testable with type t = R.t) = struct

module SM = SparseMatrix (I) (R)
open SM

let random w h : zeroFree t =
    tabulate w h (fun _ _ -> Some (T.random ()))

let rand = random (I.fromNat 10) (I.fromNat 5)

let print_matrix m =
  let () = print_endline (show m) in
  print_newline ()

let () = print_matrix rand
let () = print_matrix (transpose rand)

end
