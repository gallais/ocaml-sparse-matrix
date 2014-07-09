** Allow different indices?

  > module SparseMatrix (I : Index) (J : Index) (R : Ring) where
  This would let us have tighter types thus a better error-tracking
  system. The added cost for the user is negligible or could even
  be dismissed entirely by having a meta module instantiating both
  I and J with the same argument.

** Use phantom types to ensure [R.zero]-freeness?

  At the moment, we say explicitly in the documentation that one
  should never call [equal] on non [R.zero]-free matrices. Why
  not build that invariant in the type and give function a more
  precise type? This way we enforce [R.zero]-freeness in the
  interface of the library!
