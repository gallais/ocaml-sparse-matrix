open Index
open Ring
open SparseMatrix
open Testable

module SMII = SparseMatrix (IndexInt) (RingInt)
open SMII
module Test = TestableOption (TestableInt)

let test m n =
  let rand1 = tabulate m n (fun _ _ -> Test.random ()) in
  let rand2 = tabulate m n (fun _ _ -> Test.random ()) in
  let sum12 = plus rand1 rand2 in
  let sum21 = plus rand2 rand1 in
  assert (SMII.equal sum12 sum21)

let () =
  for i = 10 to 25 do
    for j = 15 to 30 do
      test i j;
    done
  done
