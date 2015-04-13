{-# LANGUAGE MultiParamTypeClasses #-}

module CSP (
  propagateConstraints,
  Variable(Variable),
  Location(Location),
  Game(lookupVariable,updateGame),
  Domain,
  Constraint(Alldiff,ArcConstraint)
  ) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import qualified Data.List as L

data Location = Location Int deriving (Show, Ord, Eq)

data Variable = Variable Location deriving (Show, Ord, Eq)

data Constraint a = ArcConstraint Variable Variable ([a] -> Bool)
                | KConsistent Variable ([a] -> Bool)
                | Alldiff [Variable]

type Domain = Set.Set

-- data Domain a = Domain (Set.Set a)
--             | Empty
--             deriving (Show, Eq)


class Game a b where
  lookupVariable :: b -> Variable -> Domain a
  updateGame :: (Variable,Domain a) -> b -> b

--type Game a = Map.Map Variable (Domain a)



instance Show (Constraint a) where
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

values :: Set.Set a -> [a]
values = Set.elems

instantiations :: [Domain a] -> [[a]]
instantiations domains = combinations $ map values domains


type Update a = [(Variable,Domain a)]

propagate :: (Game a game, Ord a) => Constraint a -> game -> Update a
propagate (ArcConstraint va vb fn) game =
  let da = CSP.lookupVariable game va
      db = CSP.lookupVariable game vb
      validDomains = L.transpose $ filter fn $ instantiations [da, db]
      (da':db':[]) = map domainFromList validDomains
  in [(va, da'), (vb, db')]

propagate (Alldiff variables) game =
  let diffed = allDiff $ map (CSP.lookupVariable game) variables
  in zip variables diffed

     
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
uniquify list set = let otherSets = L.delete set list
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

propagateConstraints :: (Ord a, Game a game) => [Constraint a] -> game -> game
propagateConstraints [] game = game
propagateConstraints (c:cs) game = let u = propagate c game
                                       game' = update game u
                                   in propagateConstraints cs game'

domainFromList :: Ord a => [a] -> Domain a
domainFromList [] = Set.empty
domainFromList x = Set.fromList x

update :: Game a game => game -> Update a -> game
update game [] = game
update game (u:us) = let game' = updateGame u game
                     in update game' us
