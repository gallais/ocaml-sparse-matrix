module type Testable = sig
  type t
  val self_init : unit -> unit
  val random    : unit -> t
end

module TestableInt = struct
  type t = int
  let self_init = BatRandom.self_init
  let random () = BatRandom.int 255
end

module TestableFloat = struct
  type t = float
  let self_init = BatRandom.self_init
  let random () = BatRandom.float 255.
end

module TestableOption (T : Testable) : Testable with type t = T.t option = struct
  type t = T.t option
  let self_init = T.self_init
  let random () =
    if BatRandom.int 5 == 0
    then Some (T.random ())
    else None
end
