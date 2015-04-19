import CSP

import qualified Data.Set as Set
import qualified Data.Map as Map

data Alphabet = S | E | N | D | M | O | R | Y | T | W | F | U | Null | Carry Int deriving (Eq, Ord, Show)

type Eqn = Game Alphabet Int

type EqnConstraint = Constraint Alphabet Int

eqn :: [Int] -> Bool
eqn (a:b:c:carry:[]) = a + b == c + 10 * carry
        
eqn2 (s:e:n:d:m:o:r:y:[]) =
                (s * 1000 + e * 100 + n * 10 + d)
              + (m * 1000 + o * 100 + r * 10 + e)
 == (m * 10000 + o * 1000 + n * 100 + e * 10 + y)


--   SEND
-- + MORE
---------
-- =MONEY  
constraints :: [EqnConstraint]
constraints = firstDigit:alldiff:digitConstraints

xxx :: [(Alphabet,Int)] -> Bool
xxx ((Null,a):(Null,b):(_,x0):(_,r):(_,_):[]) = x0 == r
xxx ((va,a):(vb,b):(_,x0):(vr,r):(_,x1):[]) = a + b + x0 == r + 10 * x1 &&
                                              (va == vb || a /= b) &&
                                              (va == vr || a /= r) &&
                                              (vb == vr || b /= r)

-- digitConstraints =
--   let tuples = [[D, E, Y, X1],
--                 [N, R, E, X2],
--                 [E, O, N],
--                 [S, M, O]]
--   in map (\vars -> KConstraint vars eqn) tuples
--digitConstraints :: [EqnConstraint]

--   TWO
--  +TWO
--------
-- =FOUR
digitConstraints = [
  -- KConstraint (O:O:(Carry 0):R:(Carry 1):[]) xxx,
  -- KConstraint (W:W:(Carry 1):U:(Carry 2):[]) xxx,
  -- KConstraint (T:T:(Carry 2):O:(Carry 3):[]) xxx,
  -- KConstraint (Null:Null:(Carry 3):F:(Carry 4):[]) xxx

  KVarConstraint (D:E:(Carry 0):Y:(Carry 1):[]) xxx,
  KVarConstraint (N:R:(Carry 1):E:(Carry 2):[]) xxx,
  KVarConstraint (E:O:(Carry 2):N:(Carry 3):[]) xxx,
  KVarConstraint (S:M:(Carry 3):O:(Carry 4):[]) xxx,
  KVarConstraint (Null:Null:(Carry 4):M:(Carry 5):[]) xxx  
  ]


alphabet :: [Alphabet]
alphabet = M:S:E:N:D:O:R:Y:[]

alldiff :: EqnConstraint
alldiff = Alldiff alphabet

firstDigit :: EqnConstraint
firstDigit = ArcConstraint M Null (\(m:null) -> m > 0)

--sumConstraint = KConstraint "SENDMORY" eqn2

carries = (Carry 0,[0]) : (map (flip (,) [0,1]) $ map Carry [1..5])

alphaDomain = map (flip (,) [0..9]) (drop 0 alphabet)

game :: Eqn
game = let eqnData = (Null,[0]) : (alphaDomain ++ carries)
       in makeGame eqnData

main = putStrLn $ show $ solve constraints game
