module type Testable = sig
  type t
  val self_init : unit -> unit
  val random    : unit -> t
  val randomOpt : unit -> t option (* Assumption: does not return zero! *)
end

module TestableInt = struct
  type t = int
  let self_init = BatRandom.self_init
  let random () = BatRandom.int 255
  let randomOpt () = let r = random () in if r = 0 then None else Some r
end

module TestableFloat = struct
  type t = float
  let self_init = BatRandom.self_init
  let random () = BatRandom.float 255.
  let randomOpt () = let r = random () in if r = 0. then None else Some r
end

