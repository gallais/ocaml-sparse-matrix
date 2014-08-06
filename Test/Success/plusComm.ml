open Testable
open TestableMatrixInt
let () = self_init ()

let commutes m n =
  let arg   = Some (m, Some n) in
  let rand1 = random arg       in
  let rand2 = random arg       in
  let sum12 = plus rand1 rand2 in
  let sum21 = plus rand2 rand1 in
  assert (equal (==) sum12 sum21)

let () =
  for i = 10 to 25 do
    for j = 15 to 30 do
      commutes i j;
    done
  done
