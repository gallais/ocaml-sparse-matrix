open Testable
open TestableMatrixInt
let () = self_init ()

let associative m n =
  let arg     = Some (m, Some n) in
  let rand1   = random arg in
  let rand2   = random arg in
  let rand3   = random arg in
  let sum1_23 = plus rand1 (plus rand2 rand3) in
  let sum12_3 = plus (plus rand1 rand2) rand3 in
  assert (equal (==) sum1_23 sum12_3)

let () =
  for i = 10 to 25 do
    for j = 15 to 30 do
      associative i j;
    done
  done
