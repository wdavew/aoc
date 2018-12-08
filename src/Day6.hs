module Day6 where
import Data.Maybe
import Data.List
import Text.ParserCombinators.ReadP
import qualified Data.Set as Set

type Point = (Int, Int)
type Geom = [Point]

coords :: ReadP (Int, Int)
coords = do
  x <- many1 $ satisfy (\x -> x >= '0' && x <= '9')
  string ", "
  y <- many1 $ satisfy (\x -> x >= '0' && x <= '9')
  eof
  return (read x, read y)

strToCoords :: String -> [Point]
strToCoords str =
  map (fst . head . readP_to_S coords) $
  lines str

pDist :: Point -> Point -> Int
pDist (x, y) (x', y') =
    (abs $ x - x') + (abs $ y - y')

comparepDist :: Point -> Point -> Point -> Ordering
comparepDist x a b = compare (pDist x a) (pDist x b)

strictMinBy :: (Eq a) => (a -> a -> Ordering) -> [a] -> Maybe a
strictMinBy comp xs = requireOne $
  head .
  groupBy (\a b-> (comp a b) == EQ) .
  sortBy comp $ xs
  where requireOne (x:[]) = Just x
        requireOne (x:xs) = Nothing

closestP :: Point -> [Point] -> Maybe Point
closestP p ps = strictMinBy (comparepDist p) ps

maxBySlot :: ((Int, Int) -> Int) -> [Point] -> Int
maxBySlot f = maximum . map f

minBySlot :: ((Int, Int) -> Int) -> [Point] -> Int
minBySlot f = minimum . map f

onBorder :: [Point] -> Point -> Bool
onBorder grid point = elem (fst point) [minBySlot fst grid, maxBySlot fst grid] ||
                      elem (snd point) [minBySlot snd grid, maxBySlot snd grid]

grid :: Geom -> Geom
grid g = [ (i, j) | i <- [minX..maxX], j <- [minY..maxY] ]
  where minX = minBySlot fst $ g
        maxX = maxBySlot fst $ g
        minY = minBySlot snd $ g
        maxY = maxBySlot snd $ g

areaForPoint :: Geom -> Point -> Int
areaForPoint g p = length . filter ((== p) . (fromMaybe (0, 0)) . (flip closestP g)) $ (grid g)

largestArea :: Geom -> Int
largestArea g = maximum .
  (map (areaForPoint g)) . (filter (not . (onBorder g))) $ g

inRegion :: Int -> Geom -> Point -> Bool
inRegion limit points x = (< limit) . sum . map (pDist x) $ points

regionSize :: Int -> Geom -> Int
regionSize limit g = length . filter (inRegion limit g) $ grid g
