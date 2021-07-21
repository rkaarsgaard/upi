{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeFamilies #-}

module UPichia where

import UPiBase
import UPiaBase
import UPichiaBase
import qualified UPi
import qualified UPia

import Data.Proxy
import Control.Category
import Prelude hiding (id,(.))

-- Lift and arr.
lift :: (Inhabited g) => UPia a (Times b g) -> UPichia a b
lift = FromUPia

arr' :: UPia a b -> UPichia a b
arr' f = lift (f >>> UPia.unitti)

-- Identity and composition.
instance Category UPichia where
  id = arr' id
  (FromUPia g) . (FromUPia f) = lift (f >>> g UPia.**** id >>> UPia.assoct)

-- Products of combinators.
(****) :: UPichia a b -> UPichia a' b' -> UPichia (Times a a') (Times b b')
(FromUPia f) **** (FromUPia f') = lift (f UPia.**** f' >>> UPia.arr' UPi.midswapt)

first' :: UPichia a b -> UPichia (Times a a') (Times b a')
first' c = c **** id

second' :: UPichia a b -> UPichia (Times a' a) (Times a' b)
second' c = id **** c

-- Choice.
left' :: UPichia a b -> UPichia (Plus a c) (Plus b c)
left' (FromUPia f) = lift (f UPia.++++ UPia.unitti 
                      >>> (id UPia.++++ (id UPia.**** inhab)) 
                      >>> UPia.distribri)

right' :: UPichia a b -> UPichia (Plus c a) (Plus c b)
right' f = swapp >>> left' f >>> swapp

(++++) :: UPichia a b -> UPichia a' b' -> UPichia (Plus a a') (Plus b b')
f ++++ f' = left' f >>> right' f'

merge :: (Discardable a) => UPichia (Plus a a) a
merge = arr' (UPia.unitti UPia.++++ UPia.unitti) >>> distribi >>> fst'

-- Lifted structural combinators.
unitp :: UPichia (Plus a O) a
unitp = arr' (UPia.arr' UPi.unitp)

unitpi :: UPichia a (Plus a O)
unitpi = arr' (UPia.arr' $ UPi.inv UPi.unitp)

unitt :: UPichia (Times a I) a
unitt = arr' (UPia.arr' UPi.unitt)

unitti :: UPichia a (Times a I)
unitti = arr' (UPia.arr' $ UPi.inv UPi.unitt)

swapp :: UPichia (Plus a b) (Plus b a)
swapp = arr' (UPia.arr' UPi.swapp)

swapt :: UPichia (Times a b) (Times b a)
swapt = arr' (UPia.arr' UPi.swapt)

distrib :: UPichia (Times a (Plus b c)) (Plus (Times a b) (Times a c))
distrib = arr' (UPia.arr' UPi.distrib)

distribi :: UPichia (Plus (Times a b) (Times a c)) (Times a (Plus b c))
distribi = arr' (UPia.arr' $ UPi.inv UPi.distrib)

distribo :: UPichia (Times b O) O
distribo = arr' (UPia.arr' UPi.distribo)

distriboi :: UPichia O (Times b O)
distriboi = arr' (UPia.arr' $ UPi.inv UPi.distribo)

assocp :: UPichia (Plus (Plus a b) c) (Plus a (Plus b c))
assocp = arr' (UPia.arr' UPi.assocp)

assocpi :: UPichia (Plus a (Plus b c)) (Plus (Plus a b) c)
assocpi = arr' (UPia.arr' $ UPi.inv UPi.assocp)

assoct :: UPichia (Times (Times a b) c) (Times a (Times b c))
assoct = arr' (UPia.arr' UPi.assoct)

assocti :: UPichia (Times a (Times b c)) (Times (Times a b) c)
assocti = arr' (UPia.arr' $ UPi.inv UPi.assoct)

-- Lifted derived structural combinators.
unitpl :: UPichia (Plus O a) a
unitpl = arr' (UPia.arr' UPi.unitpl)

unitpli :: UPichia a (Plus O a)
unitpli = arr' (UPia.arr' $ UPi.inv UPi.unitpl)

unittl :: UPichia (Times I a) a
unittl = arr' (UPia.arr' UPi.unittl)

unittli :: UPichia a (Times I a)
unittli = arr' (UPia.arr' $ UPi.inv UPi.unittl)

distribr :: UPichia (Times (Plus a b) c) (Plus (Times a c) (Times b c))
distribr = arr' (UPia.arr' UPi.distribr)

distribri :: UPichia (Plus (Times a c) (Times b c)) (Times (Plus a b) c)
distribri = arr' (UPia.arr' $ UPi.inv UPi.distribr)

distribol :: UPichia (Times O b) O
distribol = arr' UPia.distribol

distriboli :: UPichia O (Times O b)
distriboli = arr' (UPia.arr' $ UPi.inv UPi.distribol)

-- Discarding. 
-- Uses Kwang Yul Seo's trick (see http://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html) to get a special instance for O while avoiding doing massively inefficient things in all other cases.

type family (F a) :: Bool where
  F O  = 'True
  F a  = 'False

instance (F a ~ flag, Discardable' flag a) => Discardable a where
  discard = discard' (Proxy :: Proxy flag)

class Discardable' (flag :: Bool) a where
  discard' :: Proxy flag -> UPichia a I

instance Discardable' 'True O where
  discard' _ = arr' UPia.alloc

instance (Inhabited a) => Discardable' 'False a where
  discard' _ = lift UPia.unittli

-- Projections.
fst' :: (Discardable b) => UPichia (Times a b) a
fst' = id **** discard >>> unitt  

snd' :: (Discardable a) => UPichia (Times a b) b
snd' = discard **** id >>> unittl

-- Measurement.
measure :: (Cloneable a, Discardable a) => UPichia a a
measure = arr' clone >>> fst'