{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}

module QFC where

import UPiBase
import UPiaBase
import UPichiaBase
import qualified UPi
import UPia (inl, inr, alloc)
import qualified UPia (arr')
import UPichia hiding (measure, merge)
import qualified UPichia (measure, merge)

import Control.Category
import Prelude hiding (id,(.))

type Qbit = Plus I I
type QFC = UPichia

-- Classical states.
ket0 :: QFC I Qbit
ket0 = arr' inl

ket1 :: QFC I Qbit
ket1 = arr' inr

-- Flowchart commands.
initial :: QFC O a
initial = arr' alloc

new :: QFC a (Times a Qbit)
new = unitti >>> (id **** ket0)

discard :: QFC (Times a Qbit) a
discard = fst'

unitary :: UPi a a -> QFC a a
unitary = arr' . UPia.arr'

permute :: UPi a a -> QFC a a
permute = arr' . UPia.arr'

merge :: (Discardable a) => QFC (Plus a a) a
merge = UPichia.merge

measure :: QFC (Times a Qbit) (Plus (Times a Qbit) (Times a Qbit))
measure = (id **** (UPichia.measure)) 
            >>> distrib 
            >>> ((id **** ket0) ++++ (id **** ket1))

