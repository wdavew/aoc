module Day13 where
import Data.Map (Map, (!))
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Data.Set (Set)
import qualified Data.Map as Map

-- At intersections, carts:
--   1. turn left
--   2. go straight
--   3. turn right


-- Parser
data Track = X | Y | Cross | Turn Orientation
  deriving (Show, Eq)
data Cart = Cart Orientation Coord Intersections
  deriving (Show, Eq)
instance Ord Cart where
  compare (Cart o (x, y) s) (Cart o' (x', y') s') =
    compare (-y, x) (-y', x')
data Orientation = U | L | D | R
  deriving (Show, Eq, Ord)

type Intersections = Int
type Coord = (Int, Int)
type Board = Map Coord Track
-- spaces :: ReadP String
-- spaces = many $ char ' '

-- curve :: ReadP Track
-- curve = char '/' <|> char '\'

trackFromChar :: Char -> Maybe Track
trackFromChar '/' = Just (Turn R)
trackFromChar '\\' = Just (Turn L)
trackFromChar '+' = Just Cross
trackFromChar '-' = Just X
trackFromChar '|' = Just Y
trackFromChar '>' = Just X
trackFromChar '<' = Just X
trackFromChar '^' = Just Y
trackFromChar 'v' = Just Y
trackFromChar _ = Nothing

cartFromChar :: Coord -> Char -> Maybe Cart
cartFromChar xy '<' = Just (Cart L xy 0)
cartFromChar xy '^' = Just (Cart U xy 0)
cartFromChar xy '>' = Just (Cart R xy 0)
cartFromChar xy 'v' = Just (Cart D xy 0)
cartFromChar _ _ = Nothing

processLine :: Int -> String -> [(Coord, Track)]
processLine line =
  map (\(a, b) -> (a, fromMaybe X b)) .
  filter (isJust . snd) .
  zip [ (i, line) | i <- [0..] ] .
  map trackFromChar

processCart :: Int -> String -> [Cart]
processCart i = catMaybes . map (\(x, c) -> cartFromChar (x, i) c) . zip [0..]

allLines :: [String] -> [(Coord, Track)]
allLines ls = concatMap (\(i, j) -> processLine i j) .
  zip [0..] $ ls

allCarts :: [String] -> [Cart]
allCarts ls = concatMap (\(i, j) -> processCart i j) .
  zip [0..] $ ls

getBoard :: String -> Board
getBoard s = Map.fromList (allLines (lines s))

getCarts :: String -> Set Cart
getCarts s = Set.fromList (allCarts (lines s))

txt = readFile "./input/day13p1.txt"

-- Movement
checkCollision :: Set Cart -> Cart -> Maybe Cart
checkCollision set (Cart o (x,y) s) = find (\(Cart o' (x',y') s') -> x == x' && y == y') (sort $ Set.toList set)

moveCart :: Board -> Cart -> Cart
moveCart board c@(Cart orient (x, y) s) = trace (show c) $ case orient of
  U -> Cart U (x, y - 1) s
  D -> Cart D (x, y + 1) s
  L -> Cart L (x - 1, y) s
  R -> Cart R (x + 1, y) s

turn :: Orientation -> Orientation -> Orientation
turn L L = D
turn L R = U
turn L D = R
turn R L = U
turn R R = D
turn R D = L
turn b a = b

reOrient :: Board -> Cart -> Cart
reOrient board c@(Cart orient (x, y) s) = case (board ! (x,y)) of
  Turn R -> if (orient == U || orient == D) then Cart (turn R orient) (x,y) s else Cart (turn L orient) (x,y) s
  Turn L -> if (orient == U || orient == D) then Cart (turn L orient) (x,y) s else Cart (turn R orient) (x,y) s
  Cross -> (Cart (nextOrient s) (x, y) (s + 1))
  _ -> c
  where nextOrient s = case (mod s 3) of
          0 -> (turn L) orient
          1 -> orient
          2 -> (turn R) orient

tick :: Board -> Set Cart -> Set Cart
tick board carts = Set.fromList $ map (reOrient board . moveCart board) (sort (Set.toList carts))

checkCrashes :: Board -> Set Cart -> Cart -> Maybe Cart
checkCrashes board carts c = (checkCollision carts . reOrient board . moveCart board) c

run :: Board -> Set Cart -> Maybe Cart
run b c = head . dropWhile (isNothing) .
  concatMap (map (checkCrashes b c) . sort . Set.toList) .
  scanl' (flip tick) c $ (repeat b)
