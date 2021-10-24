module Example where

import Control.Category
import Prelude hiding (id,(.))

import UPiBase
import UPiaBase
import UPichiaBase
import UPi hiding ((****),(++++))
import UPia hiding ((****),(++++))
import UPichia

-- A slightly optimzed version of UPia.arr' . UPi.arr'.
gate :: UPi a b -> UPichia a b
gate f = FromUPia (FromUPi (UPi.unitp >>> f >>> UPi.unitti))

-- An example circuit with measurement (corresponding to examples/ex1.pdf).
ex1 = (id **** gate UPi.h **** id) >>> 
      (gate UPi.cnot **** id) >>> 
      (id **** gate UPi.h **** id) >>>
      UPichia.assoct >>>
      (id **** gate UPi.cnot) >>> 
      UPichia.assocti >>>
      (id **** id **** gate UPi.pz) >>> 
      (id **** measure **** measure)