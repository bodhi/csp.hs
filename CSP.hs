module CSP (
  propagateConstraints,
  Game,
  Constraint(..),
  variables,
  lookupVariable,
  makeGame
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.List as L

data Constraint variable value =
  ArcConstraint variable variable ([value] -> Bool)
  | KConstraint [variable] ([value] -> Bool)
  | Alldiff [variable]
  | KVarConstraint [variable] ([(variable,value)] -> Bool)

data Game variable value = Game (Map.Map variable (Set.Set value)) deriving Eq

makeGame :: (Ord variable, Ord value) => [(variable,[value])] -> Game variable value
makeGame varsVals = Game $ Map.fromList $ map (\(var, vals) -> (var, Set.fromList vals)) varsVals

lookupVariable :: Ord variable => Game variable value -> variable -> Set.Set value
lookupVariable (Game map) variable = map Map.! variable

updateGame :: Ord variable => (variable,Set.Set value) -> Game variable value -> Game variable value
updateGame (variable,domain) (Game map) = Game $ Map.insert variable domain map

variables :: Game variable value -> [variable]
variables (Game map) = Map.keys map

instance (Show variable, Ord variable, Show value) => Show (Game variable value) where
  show eqn = let alpha = variables eqn
             in foldr (\v b -> b ++ show v ++ ": " ++ show (Set.elems $ lookupVariable eqn v) ++ "\n") "" alpha

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


type Update variable value = [(variable,Set.Set value)]

propagate :: (Ord value, Ord variable) => Constraint variable value -> Game variable value -> Update variable value
propagate (ArcConstraint va vb fn) game =
  let da = CSP.lookupVariable game va
      db = CSP.lookupVariable game vb
      validDomains = L.transpose $ filter fn $ instantiations [da, db]
      (da':db':[]) = map Set.fromList validDomains
  in [(va, da'), (vb, db')]

propagate (KConstraint vars fn) game =
  let domains = map (CSP.lookupVariable game) vars
      validDomains = L.transpose $ filter fn $ instantiations domains
      newDomains = map Set.fromList validDomains
  in zip vars newDomains

propagate (Alldiff variables) game =
  let diffed = allDiff $ map (CSP.lookupVariable game) variables
  in zip variables diffed

propagate (KVarConstraint vars fn) game =
  let domains = map (CSP.lookupVariable game) vars -- [value]
      validValues = L.transpose $ filter fn $ map (zip vars) $ instantiations domains -- [[(variable, value)]]
      rawData = map unzip validValues -- [[[variable],[value]]]
       -- [([variable],[value])] -> [(variable, Set.Set value)]
      updates = map (\(vars,vals) -> (head vars, Set.fromList vals)) rawData
--      newUpdates = map (\(v, vals) -> (v, Set.fromList vals)) validUpdates
  in updates


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
uniquify list set
  | vals <= vars = let otherSets = L.delete set list
                       superset = foldl Set.union Set.empty otherSets
                   in Set.difference set superset
  | otherwise = set
  where vars = length list
        vals = Set.size $ foldr Set.union Set.empty list

-- [4],[4,5,6],[4,6,7] -> [4],[5,6],[6,7]
stripSingletons :: Ord a => [Set.Set a] -> [Set.Set a]
stripSingletons sets =
  let singletons = foldr Set.union Set.empty $ filter isSingleton sets
      strip = \set -> nonEmpty (Set.difference set singletons) set
  in map strip sets

isSingleton :: Set.Set a -> Bool
isSingleton a = Set.size a == 1

----------

propagateConstraints :: (Ord value, Ord variable) => [Constraint variable value] -> Game variable value -> Game variable value
propagateConstraints [] game = game
propagateConstraints (c:cs) game = let u = propagate c game
                                       game' = update game u
                                   in propagateConstraints cs game'

update :: Ord variable => Game variable value -> Update variable value -> Game variable value
update game [] = game
update game (u:us) = let game' = updateGame u game
                     in update game' us
