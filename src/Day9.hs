module Day9 where
import Data.Map (Map)

import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
-- TODO: redo with more lens
type Focus = Int
data FList = FList { _reverse :: [Int]
                   , _forward :: [Int]
                   , _focus :: Int
                   }
  deriving Show

data Round = Round { _circList :: FList, _score :: Int }
  deriving Show

-- A sort of doubly linked list implementation with an index (pointed list?)
moveFoc :: Int -> FList -> FList
moveFoc i (FList rev forw focus)
  | i > 0 = case (rev, forw) of
      ( [], [] ) -> FList [] [] focus
      ( _, x:ahead ) -> moveFoc (i - 1) $ FList (focus:rev) ahead x
      ( behind, [] ) -> moveFoc (i - 1) $ FList [] (reverse $ focus:init behind) (last behind)

  | i < 0 = case (rev, forw) of
      ( [], [] ) -> FList [] [] focus
      ( x:behind, _ ) -> moveFoc (i + 1) $ FList behind (focus:forw) x
      ( [], x:ahead ) -> moveFoc (i + 1) $ FList (reverse $ focus:init ahead) [] (last forw)

  | otherwise = FList rev forw focus

deleteForw :: FList -> Maybe FList
deleteForw (FList [] [] _) = Nothing
deleteForw (FList rev (a:forw) foc) = Just (FList rev forw a)
deleteForw (FList (b:rev) [] foc) = Just (FList [] rev b)

insertAhead :: Int -> FList -> FList
insertAhead x (FList rev forw foc) = FList (foc:rev) forw x

initialRound = Round (FList [] [] 0) 0

placeMarbles :: Int -> Round -> Round
placeMarbles x (Round circList score)
  | mod x 23 == 0 = trace (show ((reverse . _reverse) circList) ++ "--" ++ show (_focus circList) ++ "--" ++ show (_forward circList) ++ "\n")  Round (fromJust . deleteForw . moveFoc (-7) $ circList) ((+ x) . _focus . moveFoc (-7) $ circList)
  | otherwise = trace (show ((reverse . _reverse) circList) ++ "--" ++ show (_focus circList) ++ "--" ++ show (_forward circList) ++ "\n")  Round (insertAhead x . moveFoc 1 $ circList) 0

nRounds n init = foldl (flip placeMarbles) init $ [1..n]
scores :: Int -> Int -> Round -> Int
scores nPlayers maxVal init =
  Map.foldr max 0 .
  Map.fromListWith (+) .
  zip (map (`mod` (nPlayers)) [0..]) .
  map _score .
  scanl (flip placeMarbles) init $ [1..maxVal]

-- totalScores :: Int -> [(Int, PList)] -> Map Int Int
-- totalScores n xs = Map.fromListWith (+) $
--   zip ((map (`mod` n) [1..])) .
--   map fst $ xs

-- highestScore players turns = Map.foldl max 0 (totalScores players (scores turns))
