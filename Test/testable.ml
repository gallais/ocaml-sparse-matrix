module type Testable = sig
  type t
  type arg
  val self_init : unit -> unit
  val random    : arg option -> t
end

module TestableInt : Testable
  with type t = int and type arg = int = struct
  type t        = int
  type arg      = int
  let self_init = BatRandom.self_init
  let random v  = BatRandom.int (BatOption.default 255 v)
end

module TestableFloat : Testable
  with type t = float and type arg = float = struct
  type t        = float
  type arg      = float
  let self_init = BatRandom.self_init
  let random v  = BatRandom.float (BatOption.default 255. v)
end

open Index
open Ring

module TestableMatrixInt = struct
  module Matrix = SparseMatrix.Make (IndexInt) (RingInt)
  include Matrix
  type arg      = int * int option
  let self_init = BatRandom.self_init
  let random ov =
    let (h, ow) = BatOption.default (10, None) ov in
    let w       = BatOption.default 10 ow in
    Matrix.tabulate h w (fun i j -> TestableInt.random None)
end
