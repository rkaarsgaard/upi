{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE StandaloneDeriving #-}

module UPiBase where

import Data.Complex

-- Types in UPi.
data UPiTy = O | I | Plus UPiTy UPiTy | Times UPiTy UPiTy

-- UPi combinators extended with Hadamard and arbitrary phases.
data UPi :: UPiTy -> UPiTy -> * where
  Id :: UPi a a
  Comp :: UPi b c -> UPi a b -> UPi a c
  ProdC :: UPi a b -> UPi a' b' -> UPi (Times a a') (Times b b')
  SumC :: UPi a b -> UPi a' b' -> UPi (Plus a a') (Plus b b')
  UnitP :: UPi (Plus a O) a
  UnitPI :: UPi a (Plus a O)
  UnitT :: UPi (Times a I) a
  UnitTI :: UPi a (Times a I)
  SwapP :: UPi (Plus a b) (Plus b a)
  SwapT :: UPi (Times a b) (Times b a)
  Distrib :: UPi (Times a (Plus b c)) (Plus (Times a b) (Times a c))
  DistribI :: UPi (Plus (Times a b) (Times a c)) (Times a (Plus b c))
  DistribO :: UPi (Times b O) O
  DistribOI :: UPi O (Times b O)
  AssocP :: UPi (Plus (Plus a b) c) (Plus a (Plus b c))
  AssocPI :: UPi (Plus a (Plus b c)) (Plus (Plus a b) c)
  AssocT :: UPi (Times (Times a b) c) (Times a (Times b c))
  AssocTI :: UPi (Times a (Times b c)) (Times (Times a b) c)
  Hadamard :: UPi (Plus I I) (Plus I I)
  Phase :: (Num a, Show a) => Complex a -> UPi I I
deriving instance Show (UPi a b)
