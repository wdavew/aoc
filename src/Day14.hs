{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Day14 where

import Debug.Trace
import Data.List
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), (><))

data Elf = Elf { _seq :: Seq Int
                ,_focus :: Int
               }
  deriving Show

moveFoc :: Int -> Elf -> Elf
moveFoc i (Elf seq focus)  = Elf seq (mod (focus + i) (Seq.length seq))

append :: Seq Int -> Elf -> Elf
append x (Elf seq foc) = (Elf (seq >< x) foc)

digits :: Int -> [Int]
digits n = map (\x -> read [x]) (show n)

combineRecipes :: Int -> Int -> [Int]
combineRecipes x y = digits (x + y)

initialA = (Elf (Seq.fromList [3,7])  0)
initialB = (Elf (Seq.fromList [3,7]) 1)

moveElf :: Elf -> Elf
moveElf e = moveFoc (1 + (Seq.index (_seq e) (_focus e))) e

newRecipes :: (Elf, Elf) -> ([Int], (Elf, Elf))
newRecipes (elf1, elf2) = (newRecipes, ((moveElf . append (Seq.fromList newRecipes)) elf1,
                   (moveElf . append (Seq.fromList newRecipes)) elf2))
  where newRecipes = combineRecipes (Seq.index (_seq elf1) (_focus elf1)) (Seq.index (_seq elf2) ( _focus elf2))

recipeList :: [Int]
recipeList = 3 : (7 : (genRecipes (initialA, initialB)))
  where genRecipes(newRecipes->(xs, (a, b))) = xs ++ (genRecipes (a, b))

finalScores n = drop 10 . take n $ recipeList

firstOccurence l =  length . takeWhile (not . isPrefixOf l) $ tails recipeList
