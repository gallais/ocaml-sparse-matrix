open Index
open Ring
open SparseMatrix
open Testable

module SMII = SparseMatrix (IndexInt) (RingInt)
open SMII
open TestableInt

let () = self_init ()

let test m n =
  let rand   = tabulate m n (fun _ _ -> randomOpt ()) in
  let ttrand = transpose (transpose rand) in
  assert (SMII.equal rand ttrand)

let () =
  for i = 10 to 25 do
    for j = 15 to 30 do
      test i j;
    done
  done
