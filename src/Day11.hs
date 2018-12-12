module Day11 where
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import qualified Data.Vector as Vector
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
grid serial i =
  Map.fromList .
  map (\c -> (c, cellPower serial c)) $
  [ (x, y) | x <- [1..i], y <- [1..i] ]

square :: Int -> (Int, Int) -> [(Int, Int)]
square size (x0, y0) = [ (x, y) | y <- [ y0..y0+size-1], x <- [x0..x0+size-1] ]

maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd Nothing _ = Nothing
maybeAdd _ Nothing = Nothing
maybeAdd (Just a) (Just b) = Just (a + b)

squarePower :: Int -> (Int, Int) -> Grid -> Int
squarePower size idx grid = fromMaybe 0 . foldr (maybeAdd .
 (`Map.lookup` grid)) (Just 0) $ square size idx

compareSquares :: Int -> Grid -> (Int, Int) -> (Int, Int) -> Ordering
compareSquares size grid idx1 idx2 = compare
  (squarePower size idx1 grid)
  (squarePower size idx2 grid)

compareAll :: Grid -> ((Int, Int), Int) -> ((Int, Int), Int) -> Ordering
compareAll grid (p1, sq1) (p2, sq2) = compare
  (squarePower sq1 p1 grid)
  (squarePower sq2 p2 grid)

largestTotalPower :: Int -> Grid -> Int -> ((Int, Int), Int)
largestTotalPower size grid sq = (\ x -> (x, squarePower size x grid)) .
  maximumBy (compareSquares sq grid) $
  [ (x, y) | x <- [ sq..(size-sq)], y <- [sq..(size-sq)] ]

-- largestSquareSize :: Int -> Int -> ((Int, Int), Int)
-- largestSquareSize serial size = maximumBy (compareAll (grid serial size)) $
--   [ ((x, y), z) | x <- [ 0..size], y <- [0..size] [1..size] ]
