{-# LANGUAGE MultiParamTypeClasses #-}

import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Char
import qualified Data.Set as Set

import CSP

type Row = Int
type Col = Int

data Sudoku = Sudoku (Map.Map Variable (Domain Int)) deriving Eq

--type Sudoku = Sudoku' Int

-- class Game a b where
--   lookup :: a -> Variable -> Domain b
--   updateGame :: (Variable,Domain b) -> a -> a

instance Game Int Sudoku  where
  lookupVariable (Sudoku map) variable = map Map.! variable
  updateGame (variable,domain) (Sudoku map) = Sudoku $ Map.insert variable domain map


neq :: [Int] -> Bool
neq (x:y:[]) = x /= y

arcConstraints :: [Variable] -> [Constraint Int]
arcConstraints (d:[]) = []
arcConstraints (d:ds) = let pairs = [(d, x) | x <- ds]
                        in map arcConstraint pairs ++ arcConstraints ds

arcConstraint :: (Variable, Variable) -> Constraint Int
arcConstraint (a, b) = ArcConstraint a b neq

-------

rows :: [a] -> [[a]]
rows = splitInto 9

cols :: [a] -> [[a]]
cols = transpose . rows

-- > (map flatten) . (splitInto 2) . flatten . (map transpose) . (splitInto 2) . (splitInto 4) $ "abcdefghijklmnop"
-- ["aebf","cgdh","imjn","kolp"]
blocks :: [a] -> [[a]]
blocks = (map flatten) . (splitInto 3) . flatten . (map transpose) . (splitInto 3) . rows


splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto i a = let (first,rest) = splitAt i a
                in [first] ++ splitInto i rest

flatten :: [[a]] -> [a]
flatten = foldl (++) []

constrain :: Sudoku -> [Constraint a]
constrain game = _adConstrain game -- ++ _adConstrain game
_adConstrain game = let var = vars game
             in map Alldiff (rows var ++ cols var ++ blocks var)

_arcConstrain game = let var = vars game
            in concatMap arcConstraints (rows var ++ cols var ++ blocks var)


----------

solve :: [Constraint Int] -> Sudoku -> Sudoku
solve constraints game
  | result == game = game
  | otherwise = solve constraints result
  where result = propagateConstraints constraints game


solveGame :: Sudoku -> Sudoku
solveGame game = let constraints = constrain game
                 in solve constraints game

-------------------

vars :: Sudoku -> [Variable]
vars (Sudoku map) = Map.keys map

varify :: Int -> [Domain Int] -> [Variable]
varify i game = map (Variable . Location)  [i..length game + i - 1]

--------


freshDomain = Set.fromList [1..9]
setDomain = Set.fromList [4]

inputrow :: [Maybe Int]
inputrow = [Just 1, Nothing, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Nothing]

parseCell :: Maybe Int -> Domain Int
parseCell Nothing = freshDomain
parseCell (Just x) = Set.singleton x

parse :: [Maybe Int] -> [Domain Int]
parse x = map parseCell x

inputgame = "1..73....\
\..42....7\
\8...5...9\
\.5...8...\
\..7...38.\
\3...9.4..\
\.61.....2\
\...5.....\
\53....6.."

ps :: Char -> Domain Int
ps '.' = freshDomain
ps x = Set.singleton . Data.Char.digitToInt $ x

parseGame :: String -> Sudoku
parseGame s =
  let domains = map ps s
      dIdx = zip domains [0..]
      alist = map (\(d,i) -> (Variable (Location i), d)) dIdx
  in Sudoku $ Map.fromList alist

---

game = parseGame inputgame

strD :: Domain Int -> Char
strD d
  | size == 1 = head $ show $ head $ Set.elems d
  | otherwise = '.'
  where size = Set.size d



showG game = let vars' = vars game
                 domains = map (CSP.lookupVariable game) vars'
                 rows' = rows domains
                 showRow = \row -> (map strD row) ++ "\n"
             in concatMap showRow rows'

putG = putStrLn . showG

-------------------

main = putG $ solveGame game
