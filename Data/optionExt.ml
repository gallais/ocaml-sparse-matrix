(* A handful of helpful functions to work with the
   [option] type.*)

let liftOpt2 (f : 'a -> 'a -> 'b) (m : 'a option) (n : 'a option) =
  match m, n with
  | None, x | x, None -> x
  | Some x, Some y -> f x y

let optionElim (f : 'a -> 'b -> 'b) (ma : 'a option) (b : 'b) =
  match ma with None -> b | Some a -> f a b

let some x = Some x
