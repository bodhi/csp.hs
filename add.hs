import CSP

import qualified Data.Set as Set
import qualified Data.Map as Map

data Alphabet = S | E | N | D | M | O | R | Y | T | W | F | U | Null | Carry Int deriving (Eq, Ord, Show)

type Eqn = Game Alphabet Int

type EqnConstraint = Constraint Alphabet Int

--   SEND
-- + MORE
---------
-- =MONEY
constraints :: [EqnConstraint]
constraints = firstDigit:alldiff:digitConstraints

addColumn :: [(Alphabet,Int)] -> Bool
addColumn ((Null,a):(Null,b):(_,x0):(_,r):(_,_):[]) = x0 == r
addColumn ((va,a):(vb,b):(_,x0):(vr,r):(_,x1):[]) = a + b + x0 == r + 10 * x1 &&
                                              (va == vb || a /= b) &&
                                              (va == vr || a /= r) &&
                                              (vb == vr || b /= r)

--   TWO
--  +TWO
--------
-- =FOUR
digitConstraints = [
  -- KConstraint (O:O:(Carry 0):R:(Carry 1):[]) addColumn,
  -- KConstraint (W:W:(Carry 1):U:(Carry 2):[]) addColumn,
  -- KConstraint (T:T:(Carry 2):O:(Carry 3):[]) addColumn,
  -- KConstraint (Null:Null:(Carry 3):F:(Carry 4):[]) addColumn

  KVarConstraint (D:E:(Carry 0):Y:(Carry 1):[]) addColumn,
  KVarConstraint (N:R:(Carry 1):E:(Carry 2):[]) addColumn,
  KVarConstraint (E:O:(Carry 2):N:(Carry 3):[]) addColumn,
  KVarConstraint (S:M:(Carry 3):O:(Carry 4):[]) addColumn,
  KVarConstraint (Null:Null:(Carry 4):M:(Carry 5):[]) addColumn
  ]


alphabet :: [Alphabet]
alphabet = M:S:E:N:D:O:R:Y:[]

alldiff :: EqnConstraint
alldiff = Alldiff alphabet

firstDigit :: EqnConstraint
-- Poor man's node constraint
firstDigit = ArcConstraint M Null (\(m:null) -> m > 0)

carries = (Carry 0,[0]) : (map (flip (,) [0,1]) $ map Carry [1..5])

alphaDomain = map (flip (,) [0..9]) (drop 0 alphabet)

game :: Eqn
game = let eqnData = (Null,[0]) : (alphaDomain ++ carries)
       in makeGame eqnData

main = putStrLn $ show $ solve constraints game
