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

--   SEND
-- + MORE
---------
-- =MONEY  

--   0
--   9END
-- + 10RE
---------
-- =10NEY  

--   01
--   9END
-- + 10RE
---------
-- =10NEY  

-- 1 + E = N
-- N + R + [0,1] = E + 10 * [0,1]
-- 1 + R + [0,1] = 0 + 10 * [0,1]

-- 1 + R + [0] = 0 + 10 * [0] => 1 + R = 0 => R = -1 xx
-- 1 + R + [1] = 0 + 10 * [0] => 2 + R = 0 => R = -2 xx
-- 1 + R + [0] = 0 + 10 * [1] => 1 + R = 10 => R = 9 (xx S = 9)
-- 1 + R + [1] = 0 + 10 * [1] => 2 + R = 10 => R = 8


-- Problem is that constraints are considered in isolation. All
-- instantiations that are valid for constraint X are included, even
-- if they aren't valid for constraint Y:
-- CX: N + R + C1 = E + 10 * C2 => N + R + {0,1} = 10 + E
-- CY: C2 + E = N + 10 * C3 => 1 + E = N
--
-- C1: {0,1}
-- C2: {1}
-- C3: {0}
-- N: {3,4,5,6,7}
-- R: {4,5,6,7,8}
-- E: {2,3,4,5,6}


--N=7, R=5, E=2, C1=0 is valid for CX (so we can accept E=2, N=7)

-- E=2, N=3, is valid for CX, so we can accept E=2
-- E=6, N=7, is valid for CX, so we can accept N=7

-- But there is no way for E=2 *and* N=7, so the initial instantiation cannot actually be used!


-- So. Take constraint X. Filter out invalid instantiatons.
-- Apply instantiations to any constraints that have variables in X

-- KEY IS you need to check the instantiaton, not the updated game.


