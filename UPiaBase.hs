{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE StandaloneDeriving #-}

module UPiaBase where

import UPiBase

data UPia :: UPiTy -> UPiTy -> * where
  FromUPi :: UPi (Plus a h) b -> UPia a b
deriving instance Show (UPia a b)

class Cloneable a where
  clone :: UPia a (Times a a)

class Inhabited a where
  inhab :: UPia I a