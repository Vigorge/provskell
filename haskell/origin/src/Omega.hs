module Omega(Omega(..), Tower(..), w, (^), showCNF, unfold, isTower, latex) where

import Data.List
import Prelude hiding (exponent,(^))

data Term = Term { exponent :: Omega, multiplicity :: Integer } 
  deriving Eq
  
newtype Omega = CNFPowerSum [Term] 
  deriving Eq
  
data Tower = Tower { height :: Integer, summand :: Integer }
  deriving Eq
  
unfold :: Tower -> Omega
unfold t@(Tower h n) = unfoldReq h + nat n
  where 
    unfoldReq :: Integer -> Omega
    unfoldReq 0 = 0
    unfoldReq 1 = w 1
    unfoldReq h = w $ unfoldReq (h - 1)

zero :: Omega
zero = CNFPowerSum []

w :: Omega -> Omega
w a = CNFPowerSum [Term a 1]

nat :: Integer -> Omega
nat k 
    | k > 0  = CNFPowerSum [Term zero k]
    | k == 0 = zero
    | k < 0  = error "There are no negative ordinals"

infixr 0 `orElse`
orElse :: Ordering -> Ordering -> Ordering
c1 `orElse` c2 = if c1 /= EQ then c1 else c2

cnfCompare :: Omega -> Omega -> Ordering
cnfCompare (CNFPowerSum ds) (CNFPowerSum es) = 
    case (ds, es) of
        ([], []) -> EQ
        ([],  _) -> LT
        (_ , []) -> GT
        (d:dt,e:et) ->
                    (exponent d `cnfCompare` exponent e)
          `orElse` (multiplicity d `compare` multiplicity e)
          `orElse` (CNFPowerSum dt `cnfCompare` CNFPowerSum et)

cnfSum :: Omega -> Omega -> Omega
cnfSum alpha (CNFPowerSum []) = alpha
cnfSum (CNFPowerSum ds) (CNFPowerSum (t:ts)) = 
    let (hi,lo) = break (\d -> exponent d <= exponent t) ds in
    case lo of
        []  -> CNFPowerSum (hi ++ (t:ts))
        e:_ -> 
          if exponent e == exponent t
              then CNFPowerSum (hi ++ (t { multiplicity = multiplicity e + multiplicity t }) : ts)
              else CNFPowerSum (hi ++ (t:ts))
              
cnfMultByTerm :: Omega -> Term -> Omega
cnfMultByTerm (CNFPowerSum[]) t = 0
cnfMultByTerm (CNFPowerSum (d:ds)) (Term (CNFPowerSum[]) n) = 
    CNFPowerSum ((d { multiplicity = multiplicity d * n }):ds)
cnfMultByTerm (CNFPowerSum (d:ds)) (Term b n) = 
    CNFPowerSum [d { exponent = exponent d + b, multiplicity = n }]

cnfMult :: Omega -> Omega -> Omega
cnfMult alpha (CNFPowerSum ts) = sum [ cnfMultByTerm alpha t | t <- ts ]

isFinite :: Omega -> Bool
isFinite (CNFPowerSum[]) = True
isFinite (CNFPowerSum [Term (CNFPowerSum[]) n]) = True
isFinite   _ = False

getFinite :: Omega -> Integer 
getFinite (CNFPowerSum[]) = 0
getFinite (CNFPowerSum [Term (CNFPowerSum[]) n]) = n
getFinite _ = error "No finite ordinal given"

dlog2 n p b 
    | p >= n = (p,b)
    | otherwise = dlog2 n (2*p) (b+1)

base2 n = let (p,b) = dlog2 n 1 0 in base2rec n p 
    where 
        base2rec n 0 = []
        base2rec n p 
            | n < p = 0 : base2rec n (p `div` 2)
            | otherwise = 1 : base2rec (n - p) (p `div` 2)

powerRec :: a -> a -> (a -> a -> a) -> [Int] -> a
powerRec acc p op [] = acc
powerRec acc p op (0:bits) = powerRec acc (p `op` p) op bits
powerRec acc p op (1:bits) = powerRec (acc `op` p) (p `op` p) op bits
            
finitePow :: Omega -> Integer -> Omega
finitePow a 0 = 1
finitePow a n = 
    powerRec 1 a (*) (reverse $ base2 n)    
    
cnfExpByW :: Omega -> Omega -> Omega
cnfExpByW (CNFPowerSum []) b = 0
cnfExpByW a 0 = a
cnfExpByW a@(CNFPowerSum (Term b1 c1:bs)) b 
    | a == 1 = 1
    | a < w 1 && isFinite b = w (w (fromInteger (getFinite b - 1)))
    | a < w 1 && otherwise = w (w b)
    | otherwise = w (b1 * w b)
    
cnfExpByTerm :: Omega -> Term -> Omega
cnfExpByTerm a (Term b c) = finitePow (cnfExpByW a b) c

(^) :: Omega -> Omega -> Omega
a^(CNFPowerSum terms) = product [ cnfExpByTerm a t | t <- terms ]

instance Num Omega where 
    a + b = cnfSum a b
    a * b = cnfMult a b
    fromInteger = nat
    
    negate = undefined
    abs = undefined
    signum = undefined

instance Ord Omega where
    compare = cnfCompare

isTower :: Omega -> Bool
isTower (CNFPowerSum [Term (CNFPowerSum[]) 1]) = True
isTower (CNFPowerSum [Term a 1])               = isTower a
isTower _                                      = False

getHeight :: Omega -> Integer
getHeight (CNFPowerSum[]) = 0
getHeight (CNFPowerSum [Term a _]) = 1 + getHeight a

showTower :: Omega -> String
showTower a = "w↑" ++ show (getHeight a)

showT :: Term -> String
showT (Term (CNFPowerSum[]) n) = show n
showT (Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) 1) = "w"
showT (Term (CNFPowerSum [Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) 1]) 1) = "w^w"
showT (Term (CNFPowerSum [Term (CNFPowerSum[]) n]) 1) = "w^" ++ show n
showT (Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) k) = "w*" ++ show k
showT (Term (CNFPowerSum [Term (CNFPowerSum[]) n]) k) = "w^" ++ show n ++ "*" ++ show k
showT (Term a 1) = if isTower a then showTower a else "w^(" ++ showCNF a ++ ")"
showT (Term a n) = "w^(" ++ showCNF a ++ ")*" ++ show n

showCNF :: Omega -> String
showCNF (CNFPowerSum[]) = "0"
showCNF (CNFPowerSum terms) = 
    intercalate " + " [
        showT t | t <- terms
      ]

latexT :: Term -> String
latexT (Term (CNFPowerSum[]) n) = show n
latexT (Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) 1) = "\\omega"
latexT (Term (CNFPowerSum [Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) 1]) 1) = "\\omega^{\\omega}"
latexT (Term (CNFPowerSum [Term (CNFPowerSum[]) n]) 1) = "\\omega^" ++ show n
latexT (Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) k) = "\\omega\\cdot" ++ show k
latexT (Term (CNFPowerSum [Term (CNFPowerSum[]) n]) k) = "\\omega^" ++ show n ++ "\\cdot " ++ show k
latexT (Term a 1) = if isTower a then "\\omega\\uparrow^" ++ show (getHeight a) else "\\omega^{" ++ latex a ++ "}"
latexT (Term a n) = "\\omega^{" ++ latex a ++ "}\\cdot " ++ show n

latex :: Omega -> String
latex (CNFPowerSum[]) = "0"
latex (CNFPowerSum terms) =
    intercalate " + " [
        latexT t | t <- terms
      ]

instance Show Omega where
    show = showCNF