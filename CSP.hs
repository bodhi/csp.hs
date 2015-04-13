module CSP (
  propagateConstraints,
  lookupDomain,
  Variable(Variable),
  Location(Location),
  Game,
  Domain(Domain),
  Constraint(Alldiff,ArcConstraint)
  ) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Data.List as List

data Location = Location Int deriving (Show, Ord, Eq)

data Variable = Variable Location deriving (Show, Ord, Eq)

data Constraint = ArcConstraint Variable Variable ([Int] -> Bool)
                | KConsistent Variable ([Int] -> Bool)
                | Alldiff [Variable]


data Domain = Domain (Set.Set Int)
            | Empty
            deriving (Show, Eq)

type Game = Map.Map Variable Domain



instance Show Constraint where
  show (ArcConstraint d e _) = "<ArcConstraint " ++ (show d) ++ " & " ++ (show e) ++ ">"
  show (Alldiff d) = "<Alldiff constraint " ++ (show d) ++ ">"

-- fanOut [[1,2,3], [2,3,4]] 5 = [[1,2,3,5], [2,3,4,5]]
fanout :: [[a]] -> a -> [[a]]
fanout input suffix = map (\k -> k ++ [suffix]) input

_combinations :: [[a]] -> [[a]] -> [[a]]
_combinations out [] = out
_combinations out (i:is) =
  let out' = concatMap (fanout out) i
  in _combinations out' is

combinations :: [[a]] -> [[a]]
combinations = _combinations [[]]

values :: Domain -> [Int]
values (Domain set) = Set.elems set

instantiations :: [Domain] -> [[Int]]
instantiations domains = combinations $ map values domains


type Update = [(Variable,Domain)]

propagate :: Constraint -> Game -> Update
propagate (ArcConstraint va vb fn) game =
  let da = lookupDomain game va
      db = lookupDomain game vb
      validDomains = transpose $ filter fn $ instantiations [da, db]
      (da':db':[]) = map domainFromList validDomains
  in [(va, da'), (vb, db')]

propagate (Alldiff variables) game =
  let diffed = allDiff $ map (\(Domain s) -> s) $ map (lookupDomain game) variables
  in zipWith (\var set -> (var,Domain set)) variables diffed

     
-- eg. [4,6,9],[6,9],[6,9] -> [4],[6,9],[6,9]
allDiff :: Ord a => [Set.Set a] -> [Set.Set a]
allDiff vd =
  let vd' = stripSingletons vd
      uniquifyFn = uniquify vd'
  in map (\s -> nonEmpty (uniquifyFn s) s) vd'

nonEmpty :: Set.Set a -> Set.Set a -> Set.Set a
nonEmpty x y
  | empty = y
  | otherwise = x
  where empty = Set.null x

-- [4,6,9] -> [4,6,9], [6,9], [6,8,9] -> [4]
uniquify :: Ord a => [Set.Set a] -> Set.Set a -> Set.Set a
uniquify list set = let otherSets = delete set list
                        superset = foldl Set.union Set.empty otherSets
                    in Set.difference set superset

-- [4],[4,5,6],[4,6,7] -> [4],[5,6],[6,7]
stripSingletons :: Ord a => [Set.Set a] -> [Set.Set a]
stripSingletons sets =
  let singletons = foldr Set.union Set.empty $ filter isSingleton sets
      strip = \set -> nonEmpty (Set.difference set singletons) set
  in map strip sets

isSingleton :: Set.Set a -> Bool
isSingleton a = Set.size a == 1

----------

propagateConstraints :: [Constraint] -> Game -> Game
propagateConstraints [] game = game
propagateConstraints (c:cs) game = let u = propagate c game
                                       game' = update game u
                                   in propagateConstraints cs game'

lookupDomain :: Game -> Variable -> Domain
lookupDomain = (Map.!)


domainFromList :: [Int] -> Domain
domainFromList [] = Empty
domainFromList x = Domain $ Set.fromList x


update :: Game -> Update -> Game
update game [] = game
update game (u:us) = let game' = updateOne u game
                     in update game' us

updateOne :: (Variable,Domain) -> Game -> Game
updateOne (variable,domain) game = Map.insert variable domain game
