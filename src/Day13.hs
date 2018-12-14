module Day13 where
import Data.Map (Map, (!))
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Maybe
import Data.List
import Control.Monad
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
data Cart = Cart Orientation Coord Intersections Id
  deriving (Show, Eq)
instance Ord Cart where
  compare (Cart o (x, y) s id) (Cart o' (x', y') s' id') =
    compare (y, x, id) (y', x', id')
data Orientation = U | L | D | R
  deriving (Show, Eq, Ord)

type Id = String
type Intersections = Int
type Coord = (Int, Int)
data Board = Board { _map :: Map Coord Track, _moved :: [Cart], _next :: [Cart] }
  deriving Show

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

cartFromChar :: Coord -> Char -> String -> Maybe Cart
cartFromChar xy '<' id = Just (Cart L xy 0 id)
cartFromChar xy '^' id = Just (Cart U xy 0 id)
cartFromChar xy '>' id = Just (Cart R xy 0 id)
cartFromChar xy 'v' id = Just (Cart D xy 0 id)
cartFromChar _ _ _ = Nothing

processLine :: Int -> String -> [(Coord, Track)]
processLine line =
  map (\(a, b) -> (a, fromMaybe X b)) .
  filter (isJust . snd) .
  zip [ (i, line) | i <- [0..] ] .
  map trackFromChar

processCart :: Int -> String -> [Cart]
processCart i = mapMaybe (\(x, c) -> cartFromChar (x, i) c (show i ++ show x)) . zip [0..]

allLines :: [String] -> [(Coord, Track)]
allLines = concatMap (uncurry processLine) .
  zip [0..]

allCarts :: [String] -> [Cart]
allCarts ls = concatMap (\(i, j) -> processCart i j) .
  zip [0..] $ ls

getCarts :: String -> Set Cart
getCarts s = Set.fromList (allCarts (lines s))

getBoard :: String -> Board
getBoard s = Board (Map.fromList (allLines (lines s))) [] (sort $ Set.toList (getCarts s))

txt = readFile "./input/day13p1.txt"

-- Movement
checkCollision :: Board -> Cart -> Maybe Cart
checkCollision board c@(Cart o (x,y) s id) = find (\(Cart o' (x', y') s' id') -> x == x' && y == y' && id /= id') $
  Set.fromList (_moved board ++ _next board)

moveCart :: Board -> Cart -> Cart
moveCart board c@(Cart orient (x, y) s id) = case orient of
  U -> Cart U (x, y - 1) s id
  D -> Cart D (x, y + 1) s id
  L -> Cart L (x - 1, y) s id
  R -> Cart R (x + 1, y) s id

turn :: Orientation -> Orientation -> Orientation
turn L L = D
turn L R = U
turn L D = R
turn R L = U
turn R R = D
turn R D = L
turn b a = b

reOrient :: Board -> Cart -> Cart
reOrient board c@(Cart orient (x, y) s id) = case (_map board ! (x,y)) of
  Turn R -> if (orient == U || orient == D) then Cart (turn R orient) (x,y) s id else Cart (turn L orient) (x,y) s id
  Turn L -> if (orient == U || orient == D) then Cart (turn L orient) (x,y) s id else Cart (turn R orient) (x,y) s id
  Cross -> (Cart (nextOrient s) (x, y) (s + 1) id)
  _ -> c
  where nextOrient s = case (mod s 3) of
          0 -> (turn L) orient
          1 -> orient
          2 -> (turn R) orient

tick :: Board -> Board
tick b@(Board bmap [] []) = b
tick (Board bmap moved []) = tick $ Board bmap [] (sort moved)
tick b@(Board bmap moved (x:xs)) = Board bmap ((nextCart x):moved) xs
  where nextCart = (reOrient b . moveCart b)

findCrash :: Board -> Maybe Cart
findCrash (Board _ [] []) = Nothing
findCrash b@(Board _ [] _) = findCrash (tick b)
findCrash b@(Board bmap (x:xs) next)
  | isJust (checkCollision b x) = Just x
  | otherwise = findCrash (tick b)

crashAll :: Board -> Maybe Cart
crashAll b@(Board _ [] []) = Nothing
crashAll b@(Board _ (x:[]) []) = Just x
crashAll b@(Board _ [] (x:[])) = Just x
crashAll b@(Board _ [] next) = crashAll (tick b)
crashAll b@(Board bmap (x:xs) next)
  | isJust (checkCollision b x) = (crashAll . tick) (Board bmap (safe x xs) (safe x next))
  | otherwise = crashAll (tick b)
  where safe (Cart o (x,y) s id) = filter (\(Cart o' (x',y') s' id') -> x /= x' || y /= y')

