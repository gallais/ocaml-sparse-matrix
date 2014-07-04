open Ring
open Index
open SparseMatrix
open Testable
open Test

module TestInt   = TestSM (IndexInt) (RingInt)   (TestableInt)
module TestFloat = TestSM (IndexInt) (RingFloat) (TestableFloat)
