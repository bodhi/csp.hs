import qualified Data.Set as Set
import Data.List as List
import Data.Char

data Domain = Domain (Set.Set Int)
            | Empty
            deriving (Show, Eq)

type Row = Int
type Col = Int
data Location = Location Int deriving Show

data Variable = Variable Location deriving Show

data Constraint = ArcConstraint Variable Variable ([Int] -> Bool)
                | KConsistent [Variable] ([Int] -> Bool)
                | Alldiff [Variable]

instance Show Constraint where
  show (ArcConstraint d e _) = "<ArcConstraint " ++ (show d) ++ " & " ++ (show e) ++ ">"
  show (Alldiff d) = "<Alldiff constraint " ++ (show d) ++ ">"

-- fanOut [[1,2,3], [2,3,4]] 5 = [[1,2,3,5], [2,3,4,5]]
fanout :: [[Int]] -> Int -> [[Int]]
fanout input suffix = map (\k -> k ++ [suffix]) input

_combinations :: [[Int]] -> [[Int]] -> [[Int]]
_combinations out [] = out
_combinations out (i:is) =
  let out' = concatMap (fanout out) i
  in _combinations out' is

combinations :: [[Int]] -> [[Int]]
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

vars :: Game -> [Variable]
vars = varify 0

varify :: Int -> [Domain] -> [Variable]
varify i game = map (Variable . Location)  [i..length game + i - 1]

--------

-- _arcConstrainRows :: [[Variable]] -> [Constraint]
-- _arcConstrainRows [] = []
-- _arcConstrainRows (r:rs) = let result = arcConstraints r
--                            in result ++ _arcConstrainRows rs

-- arcConstrainRows :: [Variable] -> [Constraint]
-- arcConstrainRows game = _arcConstrainRows 0 $ rows $ varify game

-- arcConstrainCols :: [Variable] -> [Constraint]
-- arcConstrainCols game = _arcConstrainRows 0 $ cols $ varify game


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
  let domains = map (lookupDomain game) variables
      (singletons,vars) = partition (\(v,d) -> isSingleton d) $ zip variables domains
      toRemove = concatMap (\(v,Domain d) -> Set.elems d) singletons
      removeSingletons = without toRemove
      simplified = map removeSingletons domains
      diffed = allDiff vars
  in diffed


-- eg. [4,6,9],[6,9],[6,9] -> [4],[6,9],[6,9]
allDiff :: Update -> Update
allDiff vd
  | combinations <= 5000 = 
    let domains = map (\(v, d) -> d) vd
        cases = instantiations domains
        allDiffCases = filter (\l -> Set.size (Set.fromList l) == length l) cases
        domainLists = transpose allDiffCases
        newDomains = map domainFromList domainLists
    in map (\((v,oldDomain),newDomain) -> (v,newDomain)) $ zip vd newDomains
  | otherwise = vd
  where combinations = foldl (*) 1 $ map (\(v,(Domain s)) -> Set.size s) vd

without :: [Int] -> Domain -> Domain
without is (Domain d)
  | isSingleton (Domain d) = (Domain d)
  | otherwise = Domain $ foldl (\s i -> Set.delete i s) d is

isSingleton :: Domain -> Bool
isSingleton (Domain d) = Set.size d == 1

lookupDomain :: Game -> Variable -> Domain
lookupDomain game (Variable (Location l)) = game !! l

domainFromList :: [Int] -> Domain
domainFromList [] = Empty
domainFromList x = Domain $ Set.fromList x

----------

propagateConstraints :: [Constraint] -> Game -> Game
propagateConstraints [] game = game
propagateConstraints (c:cs) game = let u = propagate c game
                                       game' = update game u
                                   in propagateConstraints cs game'

constrain :: Game -> [Constraint]
constrain game = _adConstrain game ++ _arcConstrain game
_adConstrain game = let var = vars game
             in map Alldiff (rows var ++ cols var ++ blocks var)

_arcConstrain game = let var = vars game
            in concatMap arcConstraints (rows var ++ cols var ++ blocks var)


update :: Game -> Update -> Game
update game [] = game
update game (u:us) = let game' = updateOne u game
                     in update game' us

updateOne :: (Variable,Domain) -> Game -> Game
updateOne (Variable (Location l),domain) game = let (pre,_:post) = splitAt l game
                                    in pre ++ (domain:post)

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

type Game = [Domain]

-- update :: Variable -> Game -> Game
-- update (Variable domain (Location l)) game = let (pre,_:post) = splitAt l game
--                                               in pre ++ (domain:post)

-- apply :: Constraint -> Game -> Game
-- apply (ArcConstraint x y _) game = let g1 = update x game
--                                    in update y g1

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
parseGame s = map ps s

---

game = parseGame inputgame

strD :: Domain -> Char
strD (Domain d)
  | size == 1 = head $ show $ head $ Set.elems d
  | otherwise = '.'
  where size = Set.size d



showG game = let rows' = rows game
                 showRow = \row -> (map strD row) ++ "\n"
             in concatMap showRow rows'

putG = putStrLn . showG

-------------------

result = combinations [[1,2,3],[1],[1,2]]
--main = putStrLn . show $ parse inputrow
main = putG $ solveGame game
