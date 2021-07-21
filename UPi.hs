{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}

module UPi where

import Data.Complex hiding (phase)

import UPiBase
import Control.Category
import Prelude hiding (id,(.))

-- Base combinators.
instance Category UPi where
  id = Id
  Id . f = f
  f . Id = f
  g . f = Comp g f

(****) :: UPi a b -> UPi a' b' -> UPi (Times a a') (Times b b')
f **** g = ProdC f g

(++++) :: UPi a b -> UPi a' b' -> UPi (Plus a a') (Plus b b')
f ++++ g = SumC f g

unitp :: UPi (Plus a O) a
unitp = UnitP

unitpi :: UPi a (Plus a O)
unitpi = UnitPI

unitt :: UPi (Times a I) a
unitt = UnitT

unitti :: UPi a (Times a I)
unitti = UnitTI

swapp :: UPi (Plus a b) (Plus b a)
swapp = SwapP

swapt :: UPi (Times a b) (Times b a)
swapt = SwapT

distrib :: UPi (Times a (Plus b c)) (Plus (Times a b) (Times a c))
distrib = Distrib

distribi :: UPi (Plus (Times a b) (Times a c)) (Times a (Plus b c))
distribi = DistribI

distribo :: UPi (Times b O) O
distribo = DistribO

distriboi :: UPi O (Times b O)
distriboi = DistribOI

assocp :: UPi (Plus (Plus a b) c) (Plus a (Plus b c))
assocp = AssocP

assocpi :: UPi (Plus a (Plus b c)) (Plus (Plus a b) c)
assocpi = AssocPI

assoct :: UPi (Times (Times a b) c) (Times a (Times b c))
assoct = AssocT

assocti :: UPi (Times a (Times b c)) (Times (Times a b) c)
assocti = AssocTI

h :: UPi Qbit Qbit
h = Hadamard

phase :: (Num a, Show a) => Complex a -> UPi I I
phase = Phase

superposition :: UPi (Plus a a) (Plus a a)
superposition = (unitti ++++ unitti)
                  >>> distribi 
                  >>> (id **** h) 
                  >>> distrib 
                  >>> (unitt ++++ unitt)

-- Inversion.
inv :: UPi a b -> UPi b a
inv Id = Id
inv (Comp g f) = Comp (inv f) (inv g)
inv (ProdC f f') = ProdC (inv f) (inv f')
inv (SumC f f') = SumC (inv f) (inv f')
inv (Phase p) = Phase (conjugate p)
inv UnitP = UnitPI
inv UnitT = UnitTI
inv UnitPI = UnitP
inv UnitTI = UnitT
inv Distrib = DistribI
inv DistribO = DistribOI
inv DistribI = Distrib
inv DistribOI = DistribO
inv AssocP = AssocPI
inv AssocT = AssocTI
inv AssocPI = AssocP
inv AssocTI = AssocT
inv SwapP = SwapP
inv SwapT = SwapT
inv Hadamard = Hadamard

-- Derived structural combinators.
unitpl :: UPi (Plus O a) a
unitpl = swapp >>> unitp

unittl :: UPi (Times I a) a
unittl = swapt >>> unitt

distribr :: UPi (Times (Plus a b) c) (Plus (Times a c) (Times b c))
distribr = swapt >>> distrib >>> (swapt ++++ swapt)

distribol :: UPi (Times O b) O
distribol = swapt >>> distribo

midswapp :: UPi (Plus (Plus a b) (Plus c d)) (Plus (Plus a c) (Plus b d))
midswapp = assocp >>> (id ++++ (inv assocp)) 
         >>> (id ++++ (swapp ++++ id)) 
         >>> (id ++++ assocp) 
         >>> inv assocp

midswapt :: UPi (Times (Times a b) (Times c d)) (Times (Times a c) (Times b d))
midswapt = assoct >>> (id **** (inv assoct)) 
         >>> (id **** (swapt **** id)) 
         >>> (id **** assoct) 
         >>> inv assoct

-- Derived quantum gates.
type Qbit = Plus I I

i :: (Num a) => Complex a
i = 0 :+ 1

-- Pauli gates.
px :: UPi Qbit Qbit
px = swapp

py :: UPi Qbit Qbit
py = swapp >>> ((phase (-i)) ++++ (phase i))

pz :: UPi Qbit Qbit
pz = id ++++ (phase ((-1) :+ 0))

-- Phase gates.
s :: UPi Qbit Qbit
s = id ++++ (phase i)

t :: UPi Qbit Qbit
t = id ++++ (phase (exp (i * (pi/4))))

-- Controlled gates
ctrl :: UPi a a -> UPi (Times Qbit a) (Times Qbit a)
ctrl g = swapt >>> distrib 
           >>> (unitt ++++ unitt) 
           >>> (id ++++ g) 
           >>> ((inv unitt) ++++ (inv unitt)) 
           >>> inv distrib 
           >>> swapt

cz :: UPi (Times Qbit Qbit) (Times Qbit Qbit)
cz = ctrl pz

cnot :: UPi (Times Qbit Qbit) (Times Qbit Qbit)
cnot = ctrl px

toffoli :: UPi (Times Qbit (Times Qbit Qbit)) (Times Qbit (Times Qbit Qbit))
toffoli = ctrl cnot

fredkin :: UPi (Times Qbit (Times Qbit Qbit)) (Times Qbit (Times Qbit Qbit))
fredkin = ctrl swapt
