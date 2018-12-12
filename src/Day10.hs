module Day10 where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Word
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.List
import qualified Data.ByteString as Byte

-- Input parser
number :: ReadP String
number = many1 $ satisfy isDigit

negNumber :: ReadP String
negNumber = do
  char '-'
  n <- number
  return ("-" ++ n)
posNumber :: ReadP String
posNumber = do
  char ' '
  number

coordsParser :: ReadP (Coord, Coord)
coordsParser = do
  string "position=<"
  x <- negNumber <|> posNumber
  string ", "
  y <- negNumber <|> posNumber
  string "> velocity=<"
  vx <- negNumber <|> posNumber
  string ", "
  vy <- negNumber <|> posNumber
  string ">"
  eof
  return ((read x, (-1) * read y), (read vx, (-1) * read vy))

stringToCoords :: String -> (Coord, Coord)
stringToCoords s = getCoords $ readP_to_S coordsParser s
  where getCoords = fst . head

type Coord = (Int, Int)

evolve :: (Coord, Coord) -> (Coord, Coord)
evolve ( (x, y), (vx, vy) ) = ( (x + vx, y + vy), (vx, vy ) )

dist :: Coord -> Coord -> Int
dist (x, y) (x', y') = (+) (abs $ x' - x) (abs $ y' - y)

hasNeighbor :: Int -> [Coord] -> Coord -> Bool
hasNeighbor cutoff coords x = case find ((\i -> (i < cutoff && i > 0)) . \ a -> dist a x) coords of
  Nothing -> False
  Just _ -> True

evolveToMessage :: Int -> [(Coord, Coord)] -> [(Coord, Coord)]
evolveToMessage threshold coords
  | all (hasNeighbor threshold points) points = coords
  | otherwise = evolveToMessage threshold (map evolve coords)
  where points = map fst coords

-- Visualization
coordMap :: [Coord] -> Map (Int, Int) Bool
coordMap coords = Map.fromList (zip coords (repeat True))

whitePixel :: [Word8]
whitePixel = [255, 255, 255, 255]

blackPixel :: [Word8]
blackPixel = [0, 0, 0, 255]

grid :: (Int, Int) -> Int -> [(Int, Int)]
grid (x0, y0) size = concatMap (\ j -> [(i, j) | i <- [x0 - size..x0 + size]]) $ reverse [y0 - size..y0 + size]

pixels :: [(Int, Int)] -> Map (Int, Int) Bool -> [Word8]
pixels grid points =
  concatMap ((\ filled -> if filled then whitePixel else blackPixel) .
       \a -> Map.member a points) grid

sampleCoord ::  Map (Int, Int) Bool -> (Int, Int)
sampleCoord = head . Map.keys

bitmapData :: (Int, Int) -> Int -> [(Coord, Coord)] -> Byte.ByteString
bitmapData center padding = Byte.pack .
  pixels (grid center padding) .
  coordMap .
  map fst
