open Testable
open TestableMatrixInt
let () = self_init ()

let test arg =
  let rand   = random arg in
  let ttrand = transpose (transpose rand) in
  assert (equal (==) rand ttrand)
(*  try assert (equal (==) rand ttrand)
  with _ -> print rand; print ttrand; failwith "exit!" *)

let () =
  for i = 0 to 10 do test None done;
  for i = 5 to 10 do
    for j = 7 to 12 do
      test (Some (i, Some j))
    done
  done
