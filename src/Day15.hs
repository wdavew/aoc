module Day15 where
import Data.Maybe
import Data.List
import qualified Data.Set as S
import Data.Set (Set)
import Debug.Trace

newtype Coord = Coord (Int, Int)
  deriving (Show, Eq)

data Faction = Elf | Goblin
  deriving (Show, Eq)

data Unit = Unit {
  _faction :: Faction
  , _pos :: Coord
  , _hp :: Int
  , _ap :: Int
  }
  deriving (Show, Eq)

data GameState = GameState {
  _map :: Set Coord
, _units :: Set Unit
}

instance Ord Coord
  where compare (Coord (x0, y0)) (Coord (x1, y1)) = compare (-y0, x0) (-y1, x1)

compLength :: [a] -> [b] -> Ordering
compLength a b = compare (length a) (length b)

append = flip (++)

adjacent :: Coord -> Set Coord
adjacent (Coord (x0,y0)) = S.fromList [ Coord (x0 - 1, y0), Coord (x0 + 1, y0), Coord (x0, y0 - 1), Coord (x0, y0 + 1) ]

testGrid = S.fromList $ [ (i, j) | i <- [1..100]::[Int], j <- [1..100]::[Int] ]

closestTarget :: Set Coord -> Set Coord -> Coord -> Maybe Coord
closestTarget universe candidates origin = go universe candidates (S.singleton origin)
  where go valid targets exhausted
          | (not . S.null) $ S.intersection candidates exhausted = S.lookupMin (S.intersection candidates exhausted)
          | S.null (withAdjacencies exhausted S.\\ exhausted) = Nothing
          | otherwise = go universe targets (withAdjacencies exhausted)
        sOrigin = S.singleton origin
        validAdj visited point = S.intersection universe (adjacent point) S.\\ visited
        withAdjacencies visited = S.unions (map (validAdj visited) (S.toList visited))
