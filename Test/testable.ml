module type Testable = sig
  type t
  val random : unit -> t
end

module TestableInt = struct
  type t = int
  let random =
    BatRandom.self_init ();
    fun () -> BatRandom.int 255
end

module TestableFloat = struct
  type t = float
  let random =
    BatRandom.self_init ();
    fun () -> BatRandom.float 255.
end
