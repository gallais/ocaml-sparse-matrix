open Testable
open TestableMatrixInt
let () = self_init ()

let singletons m n =
  let i  = TestableInt.random (Some m)  in
  let j  = TestableInt.random (Some n)  in
  let f1 = TestableInt.random None      in
  let f2 = TestableInt.random None      in
  let v1 = singleton m n i j f1         in
  let v2 = singleton m n i j f2         in
  let v  = singleton m n i j (f1  + f2) in
  assert (equal (==) (plus v1 v2) v)

let () =
  for i = 15 to 25 do
    for j = 12 to 17 do
      singletons i j
    done
  done
