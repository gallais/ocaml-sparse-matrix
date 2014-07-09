Allow different indices?
------------------------

  > module SparseMatrix (I : Index) (J : Index) (R : Ring) where

  This would let us have tighter types thus a better error-tracking
  system. The added cost for the user is negligible or could even
  be dismissed entirely by having a meta module instantiating both
  I and J with the same argument.

  **NB:** this would force us to rethink the way we type [transpose].
  We could e.g. turn [t] into [(i, j) t] and have:

  > let transpose (m : (i, j) t) : (j, i) t

