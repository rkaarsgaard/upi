{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE StandaloneDeriving #-}

module UPichiaBase where

import UPiBase
import UPiaBase

data UPichia :: UPiTy -> UPiTy -> * where
  FromUPia :: (Inhabited g) => UPia a (Times b g) -> UPichia a b
deriving instance Show (UPichia a b)

class Discardable a where
  discard :: UPichia a I