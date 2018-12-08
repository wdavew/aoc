module Day5 where

import Data.Char

type Type = Char

data Unit = Unit Type Polarity
  deriving (Eq, Show)

data Polarity = Pos | Neg
  deriving (Eq, Show)

type Polymer = [Unit]


-- TODO - this approach still feels like an imperative approach forced into a functional style. Should redo this using free group algebra.
react :: Unit -> Unit -> Bool
react (Unit a b) (Unit c d) = a == c  && (b /= d)

odds x = map snd $ filter (odd . fst) $ zip [0..] x
evens x = map snd $ filter (even . fst) $ zip [0..] x

pairToList :: (a, a) -> [a]
pairToList (a, b) = [a, b]

reactPoly :: Int -> Polymer -> Polymer
reactPoly offset poly =
  restoreBorder .
  concatMap pairToList .
  filter (not . (uncurry react)) $
  (zip (evens $ shiftedPoly) (odds $ shiftedPoly))
  where shiftedPoly = drop offset poly
        restoreBorder x
          | offset == 1 && (even . length $ poly) = [head poly] ++ x ++ [last poly]
          | offset == 1 && (odd . length $ poly) = [head poly] ++ x
          | offset == 0 && (odd . length $ poly) = x ++ [last poly]
          | otherwise = x

fullyReducePolymer :: Polymer -> Polymer
fullyReducePolymer [] = []
fullyReducePolymer p
  | (length p) == (length (reacted p)) = p
  | otherwise = fullyReducePolymer . reacted $ p
  where reacted = reactPoly 0 . reactPoly 1

polarity :: Char -> Polarity
polarity x = case isLower x of
  True -> Pos
  False -> Neg

charToUnit :: Char -> Unit
charToUnit c = Unit (toLower c) (polarity c)

unitChar :: Unit -> Char
unitChar (Unit x _) = x

filterPolymer :: Char -> Polymer -> Polymer
filterPolymer c = filter ((/= c) . unitChar)

shortestPolymer :: Polymer -> Int
shortestPolymer x = minimum .
  map (length . fullyReducePolymer) $
  map ((flip filterPolymer) x) $
  ['a'..'z']
