{-# LANGUAGE OverlappingInstances,TypeSynonymInstances,FlexibleInstances #-}

import Data.List as List
import Data.Char
import qualified Data.Set as Set

import qualified CSP

type Row = Int
type Col = Int

type Location = Int

type Sudoku = CSP.Game Location Int

type SudokuConstraint = CSP.Constraint Location Int

-- class Game a b where
--   lookup :: a -> Variable -> Set.Set b
--   updateGame :: (Variable,Set.Set b) -> a -> a


neq :: Eq a => [a] -> Bool
neq (x:y:[]) = x /= y

pairs :: [a] -> [(a,a)]
pairs (p:[]) = []
pairs (p:ps) = [(p,s) | s <- ps] ++ pairs ps

arcConstraints :: [Location] -> [SudokuConstraint]
arcConstraints locations = map arcConstraint $ pairs locations

arcConstraint :: (Location, Location) -> SudokuConstraint
arcConstraint (a, b) = CSP.ArcConstraint a b neq

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

constrain :: Sudoku -> [SudokuConstraint]
constrain game = _adConstrain game -- ++ _adConstrain game
_adConstrain game = let var = vars game
             in map CSP.Alldiff (rows var ++ cols var ++ blocks var)

_arcConstrain game = let var = vars game
            in concatMap arcConstraints (rows var ++ cols var ++ blocks var)


----------

vars :: Sudoku -> [Location]
vars sudoku = [1..81]

--------


freshDomain = [1..9]
setDomain = Set.fromList [4]

inputrow :: [Maybe Int]
inputrow = [Just 1, Nothing, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Nothing]

parseCell :: Maybe Int -> [Int]
parseCell Nothing = freshDomain
parseCell (Just x) = [x]

parse :: [Maybe Int] -> [[Int]]
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

ps :: Char -> [Int]
ps '.' = freshDomain
ps x = [Data.Char.digitToInt $ x]

parseGame :: String -> Sudoku
parseGame s =
  let domains = map ps s
  in CSP.makeGame $ zip [1..] domains

---

game = parseGame inputgame

strD :: Set.Set Int -> Char
strD d
  | size == 1 = head $ show $ head $ Set.elems d
  | otherwise = '.'
  where size = Set.size d


instance Show Sudoku where
--instance (Show variable, Ord variable, Show value) => Show (CSP.Game Location Int) where
  show game = let vars' = vars game
                  domains = map (CSP.lookupVariable game) vars'
                  rows' = rows domains
                  showRow = \row -> (map strD row) ++ "\n"
              in concatMap showRow rows'

-------------------

constraints = constrain game
main = putStrLn $ show $ CSP.solve constraints game
