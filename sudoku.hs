import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List as List
import Data.Char

data Domain = Domain (Set.Set Int)
            | Empty
            deriving (Show, Eq)

type Row = Int
type Col = Int
data Location = Location Int deriving (Show, Ord, Eq)

data Variable = Variable Location deriving (Show, Ord, Eq)

data Constraint = ArcConstraint Variable Variable ([Int] -> Bool)
                | KConsistent [Variable] ([Int] -> Bool)
                | Alldiff [Variable]

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

neq :: [Int] -> Bool
neq (x:y:[]) = x /= y

arcConstraints :: [Variable] -> [Constraint]
arcConstraints (d:[]) = []
arcConstraints (d:ds) = let pairs = [(d, x) | x <- ds]
                        in map arcConstraint pairs ++ arcConstraints ds

arcConstraint :: (Variable, Variable) -> Constraint
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

domainFromList :: [Int] -> Domain
domainFromList [] = Empty
domainFromList x = Domain $ Set.fromList x

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

constrain :: Game -> [Constraint]
constrain game = _adConstrain game -- ++ _adConstrain game
_adConstrain game = let var = vars game
             in map Alldiff (rows var ++ cols var ++ blocks var)

_arcConstrain game = let var = vars game
            in concatMap arcConstraints (rows var ++ cols var ++ blocks var)


update :: Game -> Update -> Game
update game [] = game
update game (u:us) = let game' = updateOne u game
                     in update game' us

updateOne :: (Variable,Domain) -> Game -> Game
updateOne (variable,domain) game = Map.insert variable domain game

----------

solve :: [Constraint] -> Game -> Game
solve constraints game
  | result == game = game
  | otherwise = solve constraints result
  where result = propagateConstraints constraints game


solveGame :: Game -> Game
solveGame game = let constraints = constrain game
                 in solve constraints game

-------------------

type Game = Map.Map Variable Domain

vars :: Game -> [Variable]
vars = Map.keys

varify :: Int -> [Domain] -> [Variable]
varify i game = map (Variable . Location)  [i..length game + i - 1]

--------

lookupDomain :: Game -> Variable -> Domain
lookupDomain = (Map.!)

freshDomain = Domain $ Set.fromList [1..9]
setDomain = Domain $ Set.fromList [4]

inputrow :: [Maybe Int]
inputrow = [Just 1, Nothing, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Nothing]

parseCell :: Maybe Int -> Domain
parseCell Nothing = freshDomain
parseCell (Just x) = Domain $ Set.singleton x

parse :: [Maybe Int] -> [Domain]
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

ps :: Char -> Domain
ps '.' = freshDomain
ps x = Domain . Set.singleton . Data.Char.digitToInt $ x

parseGame :: String -> Game
parseGame s =
  let domains = map ps s
      dIdx = zip domains [0..]
      alist = map (\(d,i) -> (Variable (Location i), d)) dIdx
  in Map.fromList alist

---

game = parseGame inputgame

strD :: Domain -> Char
strD (Domain d)
  | size == 1 = head $ show $ head $ Set.elems d
  | otherwise = '.'
  where size = Set.size d



showG game = let vars' = vars game
                 domains = map (lookupDomain game) vars'
                 rows' = rows domains
                 showRow = \row -> (map strD row) ++ "\n"
             in concatMap showRow rows'

putG = putStrLn . showG

-------------------

main = putG $ solveGame game
