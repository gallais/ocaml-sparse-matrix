Allow different indices?

  > module SparseMatrix (I : Index) (J : Index) (R : Ring) where
  This would let us have tighter types thus a better error-tracking
  system. The added cost for the user is negligible or could even
  be dismissed entirely by having a meta module instantiating both
  I and J with the same argument.


