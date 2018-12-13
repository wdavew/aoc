module Day12 where
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Map (Map, (!?))
import Data.Maybe
import qualified Data.Map as Map

type Initial = String
type Rules = Map String Char
type Start = Int
data State = State { _initial :: Initial, _rules :: Rules, _start :: Start }
  deriving Show

-- Parser
pot :: ReadP Char
pot = char '#' <|> char '.'

newline :: ReadP Char
newline = char '\n'

initialState :: ReadP String
initialState = do
  string "initial state: "
  x <- many1 pot
  newline
  return x

rule :: ReadP (String, Char)
rule = do
  state <- many1 pot
  string " => "
  next <- pot
  return (state, next)

rules :: ReadP State
rules = do
  state <- initialState
  newline
  rules <- endBy1 rule newline
  eof
  return (State state (Map.fromList rules) 0)

strToState :: String -> State
strToState = fst . head . readP_to_S rules

-- Part 1

windows :: Int -> String -> [String]
windows n s = foldl zipPrepend unpacked shifts
  where zipPrepend = zipWith (flip (:))
        paddedS = s ++ (take (n-1) $ (repeat '.'))
        unpacked = map (:[]) paddedS
        shifts = take (n-1) . tail $ iterate ((:) '.') paddedS

nextState :: State -> State
nextState (State pots rules start) = State (evolve pots rules) rules (start - 2)
  where evolve p r = map (\x -> fromMaybe '.' $ r !? x) . windows 5 $ p

run :: Int -> State -> State
run n state = last . take n $ iterate nextState state

plantedPots :: State -> Int
plantedPots (State pots rules start) =
  sum .
  map snd .
  filter ((==) '#' . fst) $
  zip pots [start ..]

plantSum :: State -> Int -> Int
plantSum state n = last $ map (plantedPots . flip run state) [1..n]

lead :: Int -> [Int] -> [[Int]]
lead n l = foldl (\a b -> zipWith (++) a (drop b $ map (:[]) l)) (map (:[]) l) [1..n]

runningDiff :: [Int] -> [Int]
runningDiff l = zipWith (-) (tail l) l

findRepeat :: State -> (Int, Int)
findRepeat state = (\x -> (x, runningDiff totals !! x)) .
  (fst . head) .
  dropWhile ((any (/= 0)) . snd) .
  zip [0..] .
  lead 5 .
  (runningDiff . runningDiff) $ totals
  where
    totals = map (plantedPots . flip run state) [1..]

bigPlantSum :: State -> Int -> Int
bigPlantSum s n = initial + max (n - (fst repeat) - 1) 0 * (snd repeat)
  where repeat = findRepeat s
        initial = plantSum s (min n ((fst repeat) + 1))
