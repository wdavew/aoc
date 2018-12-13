module Day11 where
import Data.Map (Map, (!))
import Data.List
import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe

type Cell = (Int, Int)
type Square = [Cell]

type Grid = Map Cell Int

cellId (x, y) = x + 10

nthDigit x = (`mod` 10) . (`quot` round (10 ** x))

cellPower :: Int -> Cell -> Int
cellPower serial c@(x, y) =
  (+) (-5) .
  nthDigit 2 .
  (*) (cellId c) .
  (+) serial .
  (* y) .
  cellId $ c

grid :: Int -> Int -> Grid
grid serial n =
  Map.fromList .
  zipWith (,) [ (x, y) | y <- ([1..n]), x <- [1..n] ] .
  concat $
  scanl1 (zipWith (+)) (map row ([1..n]))
  where row i = scanl1 (+) [ cellPower serial (x, i) | x <- [1..n] ]

squarePower :: Int -> (Int, Int) -> Grid -> Int
squarePower size (x, y) grid
  | x == 1 && y == 1 =  grid ! (x + size - 1, y + size - 1)
  | y == 1 =  grid ! (x + size - 1, y + size - 1) - grid ! (x,  y + size - 1)
  | x == 1 =  grid ! (x + size - 1, y + size - 1) - grid ! (x + size - 1, y)
  | otherwise =  (grid ! (x - 1, y - 1)) - (grid ! (x + size - 1, y - 1)) - (grid ! (x - 1, y + size - 1)) + (grid ! (x + size - 1, y + size - 1))

compareSquares :: Int -> Grid -> (Int, Int) -> (Int, Int) -> Ordering
compareSquares size grid idx1 idx2 = compare
  (squarePower size idx1 grid)
  (squarePower size idx2 grid)

compareAll :: (((Int, Int), Int), Int) -> (((Int, Int), Int), Int) -> Ordering
compareAll p1 p2 = compare (snd . fst $ p1) (snd . fst $ p2)

largestTotalPower :: Int -> Int -> Int -> ((Int, Int), Int)
largestTotalPower size sq serial = (\ x -> (x, squarePower sq x g)) .
  maximumBy (compareSquares sq g) $
  [ (x, y) | x <- [1..size-sq+1], y <- [1..size-sq+1] ]
  where g = grid serial size

largestSquareSize :: Int -> Int -> (((Int, Int), Int), Int)
largestSquareSize serial size = maximumBy compareAll $
  map (\x -> (largestTotalPower size x serial, x)) $
  [1..size]
  where g = grid serial size
