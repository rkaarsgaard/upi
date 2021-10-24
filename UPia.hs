{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}

module UPia where

import UPiBase
import UPiaBase
import qualified UPi

import Control.Category
import Prelude hiding (id,(.))

-- Lift and arr.
lift :: UPi (Plus a h) b -> UPia a b
lift = FromUPi

arr' :: UPi a b -> UPia a b
arr' f = lift (UPi.unitp >>> f)

-- Identity and composition.
instance Category UPia where
  id = arr' id
  (FromUPi g) . (FromUPi f) = lift ((UPi.inv UPi.assocp) >>> (f UPi.++++ id) >>> g)

-- Products and sums of combinators.
(****) :: UPia a b -> UPia a' b' -> UPia (Times a a') (Times b b')
(FromUPi f) **** (FromUPi f') = lift $ UPi.inv UPi.assocp 
                              >>> (UPi.inv UPi.distrib) UPi.++++ (UPi.inv UPi.distrib)
                              >>> UPi.inv UPi.distribr
                              >>> f UPi.**** f'

first' :: UPia a b -> UPia (Times a a') (Times b a')
first' c = c **** id

second' :: UPia a b -> UPia (Times a' a) (Times a' b)
second' c = id **** c

(++++) :: UPia a b -> UPia a' b' -> UPia (Plus a a') (Plus b b')
(FromUPi f) ++++ (FromUPi f') = lift (UPi.midswapp >>> f UPi.++++ f')

left' :: UPia a b -> UPia (Plus a a') (Plus b a')
left' c = c ++++ id

right' :: UPia a b -> UPia (Plus a' a) (Plus a' b)
right' c = id ++++ c

-- Lifted structural combinators.
unitp :: UPia (Plus a O) a
unitp = arr' UPi.unitp

unitpi :: UPia a (Plus a O)
unitpi = arr' (UPi.inv UPi.unitp)

unitt :: UPia (Times a I) a
unitt = arr' UPi.unitt

unitti :: UPia a (Times a I)
unitti = arr' (UPi.inv UPi.unitt)

swapp :: UPia (Plus a b) (Plus b a)
swapp = arr' UPi.swapp

swapt :: UPia (Times a b) (Times b a)
swapt = arr' UPi.swapt

distrib :: UPia (Times a (Plus b c)) (Plus (Times a b) (Times a c))
distrib = arr' UPi.distrib

distribi :: UPia (Plus (Times a b) (Times a c)) (Times a (Plus b c))
distribi = arr' (UPi.inv UPi.distrib)

distribo :: UPia (Times b O) O
distribo = arr' UPi.distribo

distriboi :: UPia O (Times b O)
distriboi = arr' (UPi.inv UPi.distribo)

assocp :: UPia (Plus (Plus a b) c) (Plus a (Plus b c))
assocp = arr' UPi.assocp

assocpi :: UPia (Plus a (Plus b c)) (Plus (Plus a b) c)
assocpi = arr' (UPi.inv UPi.assocp)

assoct :: UPia (Times (Times a b) c) (Times a (Times b c))
assoct = arr' UPi.assoct

assocti :: UPia (Times a (Times b c)) (Times (Times a b) c)
assocti = arr' (UPi.inv UPi.assoct)

-- Lifted derived structural combinators.
unitpl :: UPia (Plus O a) a
unitpl = arr' UPi.unitpl

unitpli :: UPia a (Plus O a)
unitpli = arr' (UPi.inv UPi.unitpl)

unittl :: UPia (Times I a) a
unittl = arr' UPi.unittl

unittli :: UPia a (Times I a)
unittli = arr' (UPi.inv UPi.unittl)

distribr :: UPia (Times (Plus a b) c) (Plus (Times a c) (Times b c))
distribr = arr' UPi.distribr

distribri :: UPia (Plus (Times a c) (Times b c)) (Times (Plus a b) c)
distribri = arr' (UPi.inv UPi.distribr)

distribol :: UPia (Times O b) O
distribol = arr' UPi.distribol

distriboli :: UPia O (Times O b)
distriboli = arr' (UPi.inv UPi.distribol)

-- Allocation.
alloc :: UPia O a
alloc = lift (UPi.swapp >>> UPi.unitp )

-- Injections
inl :: UPia a (Plus a b)
inl = lift id

inr :: UPia b (Plus a b)
inr = lift UPi.swapp

-- Classical cloning
instance Cloneable O where
  clone = distriboli

instance Cloneable I where
  clone = unitti

instance (Cloneable a, Cloneable b) => Cloneable (Times a b) where
  clone = (clone **** clone) >>> arr' UPi.midswapt

instance (Cloneable a, Cloneable b) => Cloneable (Plus a b) where
  clone = (clone ++++ clone) >>> ((inl **** id) ++++ (inr **** id)) >>> distribi

-- Inhabitation.
instance Inhabited I where
  inhab = id

instance (Inhabited a, Inhabited b) => Inhabited (Times a b) where
  inhab = unitti >>> inhab **** inhab

instance (Inhabited a) => Inhabited (Plus a b) where
  inhab = inhab >>> inl
