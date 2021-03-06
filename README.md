Quantum Information Effects
---------------------------

This is an implementation of the languages <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi">, <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi_a">, and <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi^\chi_a"> and all related constructions, described in the paper [Quantum Information Effects](https://arxiv.org/abs/2107.12144) by Chris Heunen and Robin Kaarsgaard.

This directory contains the implementation of the constructions and
translations relating to <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi">, <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi_a">, and <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi^\chi_a">. These languages are implemented as eDSLs in (heavily extended Glasgow) Haskell. They have been tested to work with the GHC Haskell compilation system version 8.10.1 on macOS 11.6 Big Sur, as well as 8.6.5 on Ubuntu 20.04.

Though Haskell supports arrows via the `Arrow` type class, the implementation in Haskell only permits arrows over Haskell functions (i.e., over the type `a -> b`) rather than over an arbitrary `Category` instance. For this reason, though the <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi_a"> and <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi^\chi_a"> constructions *are* arrows, they cannot be implemented as such. Instead, we have chosen to name the arrow combinators with suggestive but non-conflicting names, such as `arr'`, `first'`, `left'`, and so forth.

This repository is structured as follows:
* `UPiBase.hs`: Contains the data type declaration for <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi"> combinators.
* `UPi.hs`: Implementation of all (derived) <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi"> combinators, including the quantum gates described in Section 3.2.
* `UPiaBase.hs`: Contains the data type declaration for <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi_a"> combinators, as well as the declaration of the type classes `Cloneable` and `Inhabited` used to define the `clone` and `inhab` combinators respectively. Since all <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi"> types have a `Cloneable` instance, the `Cloneable` constraint is trivial (but Haskell doesn't know that).
* `UPia.hs`: Contains the implementation of all (derived) combinators of <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi_a">, as described in Section 3.3.
* `UPichiaBase.hs`: Contains the data type declaration for <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi^\chi_a"> combinators, as well as the declaration of the `Discardable` type class used to handle projections. Again, all <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi"> types have `Discardable` instances, so the constraint is trivial, but Haskell doesn't know that.
* `UPichia.hs`: Contains the implementation of all (derived) combinators relating to <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi^\chi_a">, see Section 3.4.
* `QFC.hs`: Contains the translation from quantum flow charts to <img src="https://render.githubusercontent.com/render/math?math=\mathcal{U}\Pi^\chi_a"> as described in Section 6.2.

Known shortcomings
------------------
 * In the paper, a straightforward way of deriving `Inhabited b+b'` from `Inhabited b'` is described. However, this is not implemented due to overlap with the instance deriving `Inhabited b+b'` from `Inhabited b`.


