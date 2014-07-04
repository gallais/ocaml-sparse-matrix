ocaml-sparse-matrix
===================

Implementation of Sparse Matrices in Ocaml using Batteries
-------------------

* In order to be as general as possible, we define an indexed functor
  `SparseMatrix : Index -> Ring -> Set` thus giving us the possibility
  to index elements either by using `Pervasives`' limited precision `int`,
  `Batteries`' `Big_int` or any other index set with the right structure.


License
-------

```
The content of this repository is licensed under the WTFPL:

           DO WHATEVER THE FUCK YOU WANT, PUBLIC LICENSE
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

            0. You just DO WHATEVER THE FUCK YOU WANT.
```
