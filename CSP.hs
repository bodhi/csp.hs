{-# LANGUAGE MultiParamTypeClasses #-}

module CSP (
  propagateConstraints,
  Game(lookupVariable,updateGame),
  Constraint(Alldiff,ArcConstraint)
  ) where

import qualified Data.Set as Set

import qualified Data.List as L

data Constraint variable a = ArcConstraint variable variable ([a] -> Bool)
--                | KConsistent variable ([a] -> Bool)
                | Alldiff [variable]

class Game a b variable where
  lookupVariable :: b -> variable -> Set.Set a
  updateGame :: (variable,Set.Set a) -> b -> b

-- instance Show (Constraint var a) where
--   show (ArcConstraint d e _) = "<ArcConstraint " ++ (show d) ++ " & " ++ (show e) ++ ">"
--   show (Alldiff d) = "<Alldiff constraint " ++ (show d) ++ ">"

----

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

instantiations :: [Set.Set a] -> [[a]]
instantiations domains = combinations $ map Set.elems domains


type Update var a = [(var,Set.Set a)]

propagate :: (Game a game var, Ord a) => Constraint var a -> game -> Update var a
propagate (ArcConstraint va vb fn) game =
  let da = CSP.lookupVariable game va
      db = CSP.lookupVariable game vb
      validDomains = L.transpose $ filter fn $ instantiations [da, db]
      (da':db':[]) = map Set.fromList validDomains
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

propagateConstraints :: (Ord a, Game a game var) => [Constraint var a] -> game -> game
propagateConstraints [] game = game
propagateConstraints (c:cs) game = let u = propagate c game
                                       game' = update game u
                                   in propagateConstraints cs game'

update :: Game a game var => game -> Update var a -> game
update game [] = game
update game (u:us) = let game' = updateGame u game
                     in update game' us
