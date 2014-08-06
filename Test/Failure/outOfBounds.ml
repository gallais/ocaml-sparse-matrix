open Index
open Ring
module Matrix = SparseMatrix.Make (IndexInt) (RingFloat)
open Matrix

exception WasSuccessful

let hasFailed e =
  try let _ = e () in raise WasSuccessful
  with Invalid_argument _ -> ()

let () =
  List.iter hasFailed
  [ fun () -> set 14 9 6. (zero 3  10)
  ; fun () -> set 10 6 3. (zero 11 6)
  ; fun () -> get 3 4 (zero 10 3)
  ; fun () -> get 78 0 (zero 1 2) ]

