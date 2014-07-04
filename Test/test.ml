module TestSM (I : Index) (R : Ring)
  (T : Testable with type t = R.t) = struct

module SM = SparseMatrix (I) (R)
open SM

let random w h : t = tabulate w h (fun _ _ -> T.random ())

let rand = random (I.fromNat 10) (I.fromNat 5)

let print_matrix m =
  let () = print_endline (show m) in
  print_newline ()

let () = print_matrix rand
let () = print_matrix (transpose rand)

end

module TestInt   = TestSM (IndexInt) (RingInt)   (TestableInt)
module TestFloat = TestSM (IndexInt) (RingFloat) (TestableFloat)
