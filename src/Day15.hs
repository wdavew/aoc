module Day15 where
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Data.Set (Set)
import Debug.Trace

-- Data Structures

newtype Coord = Coord (Int, Int)
  deriving (Show, Eq)

data Faction = Elf | Goblin
  deriving (Show, Eq)

data Unit = Unit {
   _faction :: Faction
 , _pos :: Coord
 , _hp :: Int
 , _ap :: Int
 , _id :: Int
}
  deriving (Show, Eq)

data GameState = GameState {
  _map :: Set Coord
, _walls :: Set Coord
, _units :: Map Int Unit
, _next :: [Int]
, _resolved :: [Int]
, _rounds :: Int
, _complete :: Bool
}
  deriving (Show)

instance Ord Coord
  where compare (Coord (x0, y0)) (Coord (x1, y1)) = compare (y0, x0) (y1, x1)

instance Ord Unit
  where compare u u' = compare (_pos u) (_pos u')

compLength :: [a] -> [b] -> Ordering
compLength a b = compare (length a) (length b)

compSnd :: Ord b => (a, b) -> (a, b) -> Ordering
compSnd a b = compare (snd a) (snd b)

compUnitHp :: Unit -> Unit -> Ordering
compUnitHp a b = compare (_hp a, y0, x0) (_hp b, y1, x1)
  where (Coord (x0, y0)) = _pos a
        (Coord (x1, y1)) = _pos b

-- Input parsing and printing
toUnit :: Char -> Coord -> (Int, Unit)
toUnit 'E' p@(Coord (x,y)) = ((uid x y), Unit Elf p 200 3 (uid x y))
  where uid x y = (read $ (show x) ++ (show y))
toUnit 'G' p@(Coord (x,y)) = ((uid x y), Unit Goblin p 200 3 (uid x y))
  where uid x y = (read $ (show x) ++ (show y))
toUnit _ _ = error "Not a valid unit"

strToBoard :: (Int, String) -> GameState -> GameState
strToBoard (y, str) gs  = gs { _map = (S.union (_map gs) coords)
                       , _walls = (S.union (_walls gs) walls)
                       , _units = (M.union (_units gs) units)
                       }
  where inpt = zip str [Coord (i, y) | i <- [1..]]
        coords = S.fromList $ map snd inpt
        walls = S.fromList $ map snd . filter ((==) '#' . fst) $ inpt
        units = M.fromList $ map (uncurry toUnit) . filter ((`elem` "EG") . fst) $ inpt

linesToBoard :: GameState -> [String] -> GameState
linesToBoard init s = foldr strToBoard init (zip [1..] s)

printBoard :: GameState -> String
printBoard gameState = unlines .
  chunksOf maxX .
  map fst . sortBy compSnd $ allSpaces
  where allSpaces = S.toList $ S.unions [allUnits, allWalls, emptySpace]
        allUnits = S.map (\x -> (if (_faction x) == Elf then 'E' else 'G', _pos x)) (S.fromList $ map snd (M.toList $ _units gameState))
        allWalls = S.map (\x -> ('#', x)) (_walls gameState)
        emptySpace = S.map (\x -> ('.', x)) (_map gameState S.\\ (S.union (_walls gameState) (S.fromList $ (map (_pos . snd) (M.toList $ _units gameState)))))
        maxX = maximum $ map ((\(Coord (x, y)) -> x) . snd) allSpaces

-- Pathing and targeting
adjacent :: Coord -> Set Coord
adjacent (Coord (x0,y0)) = S.fromList [ Coord (x0 - 1, y0), Coord (x0 + 1, y0), Coord (x0, y0 - 1), Coord (x0, y0 + 1) ]

closestTarget :: Set Coord -> Set Coord -> Coord -> Maybe Coord
closestTarget universe candidates origin = go universe candidates (S.singleton origin)
  where go valid targets exhausted
          | (not . S.null) $ S.intersection candidates exhausted = S.lookupMin (S.intersection candidates exhausted)
          | withAdjacencies exhausted == S.empty = Nothing
          | otherwise = go universe targets (S.union exhausted (withAdjacencies exhausted))
        sOrigin = S.singleton origin
        validAdj visited point = (S.intersection universe (adjacent point)) S.\\ visited
        withAdjacencies visited = S.unions (map (validAdj visited) (S.toList visited))

moveUnit :: Unit -> Coord -> Unit
moveUnit u newPos = u { _pos = newPos }

passable :: GameState -> Faction -> Set Coord
passable gs fac = _map gs S.\\
  S.union (_walls gs) (S.map _pos (S.filter ((== fac) . _faction) (S.fromList (map snd (M.toList $ _units gs)))))

allPassable :: GameState -> Set Coord
allPassable gs = _map gs S.\\ S.union (_walls gs) (S.map _pos (S.fromList (map snd (M.toList $ _units gs))))

enemyInRange :: GameState -> Unit -> Maybe Unit
enemyInRange gs u = case S.toList $ S.filter ((`elem` adjacencies) . _pos) enemies of
  [] -> Nothing
  x -> Just $ minimumBy compUnitHp $ x
  where adjacencies = S.intersection (adjacent (_pos u)) (passable gs (_faction u))
        enemies = S.filter ((/= _faction u) . _faction) $ S.fromList (map snd (M.toList $ _units gs))

findTarget :: GameState -> Unit -> Set Unit -> Maybe Coord
findTarget gs u enemies = closestTarget (allPassable gs) adjEnemySquares (_pos u)
  where adjEnemySquares = S.intersection (S.unions $ map (adjacent . _pos) (S.toList enemies)) (allPassable gs)

-- Actions
attackUnit :: Unit -> Unit -> [Unit]
attackUnit unit target = [unit, target { _hp = _hp target - _ap unit }]

enemies :: GameState -> Unit -> Set Unit
enemies gs u = S.filter ((/= _faction u) . _faction) (S.fromList $ map snd (M.toList $ _units gs))

nextMove :: GameState -> Unit -> Maybe Coord
nextMove gs u = case findTarget gs u (enemies gs u) of
  Nothing -> Nothing
  (Just targetPos) -> closestTarget (passable gs (_faction u)) validAdj targetPos
  where valid = passable gs (_faction u)
        validAdj = S.intersection valid (adjacent (_pos u))

cleanupDead :: GameState -> GameState
cleanupDead gs = gs { _units = alive }
  where alive = M.filter ((> 0) . _hp) (_units gs)

updateUnits :: GameState -> [Unit] -> GameState
updateUnits gs@(GameState { _units = u }) updated = gs { _units = M.union newUnits u }
  where newUnits = M.fromList $ map (\x -> (_id x, x)) updated

-- Gameplay
nextTurn :: GameState -> GameState
nextTurn gs@GameState { _next = x:xs, _resolved = ys } = gs { _next = xs, _resolved = x:ys }
nextTurn gs@GameState { _next = [], _resolved = [] } = gs { _next = (map fst) $ sortBy compSnd (M.toList $ _units gs) }

resolveAttacks :: Unit -> GameState -> GameState
resolveAttacks u gs = case enemyInRange gs u of
  (Just enemy) -> cleanupDead . updateUnits gs $ attackUnit u enemy
  Nothing -> gs

resolveMoves :: Unit -> GameState -> (Unit, GameState)
resolveMoves u gs = case enemyInRange gs u of
  (Just enemy) -> (u, gs)
  Nothing -> case nextMove gs u of
    (Just targetPos) -> (\x -> (x, updateUnits gs [x])) . moveUnit u $ targetPos
    Nothing -> (u, gs)

resolveUnit :: Unit -> GameState -> GameState
resolveUnit u = uncurry resolveAttacks . resolveMoves u

testGrid = S.fromList [ (i, j) | i <- [1..100]::[Int], j <- [1..100]::[Int] ]
emptyState = GameState S.empty S.empty M.empty [] [] 0 False

nextRound gs@GameState {
   _resolved = ys
 , _rounds = i
 , _units = u
 , _complete = False
 } = gs { _next = (map fst) . (sortBy compSnd) $ (M.toList u), _resolved = [], _rounds = i + 1 }
nextRound gs@GameState { _complete = True } = gs

getUnit :: Int -> GameState -> Maybe Unit
getUnit id gs = M.lookup id (_units gs)

resolveRound :: GameState -> GameState
resolveRound gs@GameState { _next = [] } = gs
resolveRound gs@GameState { _next = x:xs } = case getUnit x gs of
  Nothing -> resolveRound . nextTurn $ gs
  (Just u) -> case gameOver u gs of
    True -> gs { _complete = True }
    False -> resolveRound . nextTurn . resolveUnit u $ gs

gameOver :: Unit -> GameState -> Bool
gameOver u gs = case length $ S.toList (enemies gs u) of
    0 -> True
    _ -> False

summarizeGame :: GameState -> (Int, Int)
summarizeGame GameState { _units = units, _rounds = r } = (sum (map (_hp . snd) $ M.toList units), r)

txt = readFile "./input/day15.txt"
