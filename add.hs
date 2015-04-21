{-# LANGUAGE DeriveDataTypeable #-}

import CSP

import qualified Data.List as L
import Data.Data

data Alphabet = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q |R | S | T | U | V | W | X | Y | Z | Null | Carry Int deriving (Eq, Ord, Show, Typeable, Data)

type Eqn = Game Alphabet Int

type EqnConstraint = Constraint Alphabet Int

--------------------

addColumn :: [(Alphabet,Int)] -> Bool
addColumn ((Null,_):(Null,_):(_,x0):(_,r):(_,_):[]) = x0 == r
addColumn ((va,a):(vb,b):(_,x0):(vr,r):(_,x1):[]) = a + b + x0 == r + 10 * x1 &&
                                              (va == vb || a /= b) &&
                                              (va == vr || a /= r) &&
                                              (vb == vr || b /= r)

--------------------

columnConstraints :: [Alphabet] -> [Alphabet] -> [Alphabet] -> [EqnConstraint]
columnConstraints a b c =
  let toConstraint = \x y z col -> columnConstraint col (x,y) z
      (a':b':c':[]) = map reverse [a, b, c]
  in reverse $ L.zipWith4 toConstraint a' b' c' [0..]

--  a
-- +b
-----
-- =c
columnConstraint :: Int -> (Alphabet,Alphabet) -> Alphabet -> EqnConstraint
columnConstraint column (a, b) c = KVarConstraint [a, b, (Carry column), c, (Carry $ column + 1)] addColumn

firstDigit :: Alphabet -> EqnConstraint
-- Poor man's node constraint
firstDigit a = ArcConstraint a Null (\(m:_) -> m > 0)

--------------------

carries n = (Carry 0,[0]) : (map (flip (,) [0,1]) $ map Carry [1..n])

alphaDomain alphabet = map (flip (,) [0..9]) alphabet

game :: [Alphabet] -> Int -> Eqn
game alphabet columns = let eqnData = (Null,[0]) : (alphaDomain alphabet ++ carries columns)
       in makeGame eqnData

--------------------

decodeChar :: Char -> Maybe Alphabet
decodeChar c = fromConstr <$> readConstr (dataTypeOf Null) [c]

decode :: String -> Maybe [Alphabet]
decode s = sequence $ map decodeChar s

fillLeft :: Int -> [Alphabet] -> [Alphabet]
fillLeft len a
  | toFill > 0 = replicate toFill Null ++ a
  | otherwise = a
  where toFill = len - (length a)

mkGame :: [Alphabet] -> [Alphabet] -> [Alphabet] -> (Eqn, [EqnConstraint])
mkGame a b c =
  let len = length c
      alphabet = a ++ b ++ c
      (a':b':[]) = map (fillLeft len) [a,b]
  in (game alphabet len, Alldiff alphabet:firstDigit (head c):columnConstraints a' b' c)

decodeGame :: String -> String -> String -> Maybe (Eqn, [EqnConstraint])
decodeGame a b c = do (a':b':c':[]) <- sequence $ map decode [a,b,c]
                      pure $ mkGame a' b' c'

main = let Just (game,constraints) = decodeGame "SEND" "MORE" "MONEY"
       in putStrLn $ show $ solve constraints game
