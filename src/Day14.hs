{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Day14 where

import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), (><))

type Focus = Int
data Elf = Elf { _reverse :: Seq Int
                   , _forward :: Seq Int
                   , _focus :: Int
                   }
  deriving Show

moveFoc :: Int -> Elf -> Elf
moveFoc i (Elf rev forw focus)
  | i > 0 = case (rev, forw) of
      ( Seq.Empty, Seq.Empty ) -> Elf Seq.empty Seq.empty focus
      ( _, Seq.viewl -> (next Seq.:< rest) ) -> moveFoc (i - 1) $ Elf (focus <| rev) rest next
      ( Seq.viewr -> (rest Seq.:> last), Seq.Empty ) -> moveFoc (i - 1) $ Elf (Seq.empty) (Seq.reverse (focus <| rest)) last
  | i < 0 = case (rev, forw) of
      ( Seq.Empty, Seq.Empty ) -> Elf Seq.empty Seq.empty focus
      ( Seq.viewl -> prev Seq.:< rest, _ ) -> moveFoc (i + 1) $ Elf rest (focus <| forw) prev
      ( Seq.Empty, Seq.viewr -> (rest Seq.:> last )) -> moveFoc (i + 1) $ Elf (Seq.reverse $ focus <| rest) Seq.Empty last
  | otherwise = Elf rev forw focus

append :: Seq Int -> Elf -> Elf
append x (Elf rev forw foc) = (Elf rev (forw >< x) foc)

digits :: Int -> [Int]
digits n = map (\x -> read [x]) (show n)

combineRecipes :: Int -> Int -> [Int]
combineRecipes x y = digits (x + y)

initialA = (Elf Seq.empty (Seq.singleton 7)  3)
initialB = (Elf (Seq.singleton 3) Seq.empty 7)

moveElf :: Elf -> Elf
moveElf e = moveFoc (1 + _focus e) e

newRecipes :: (Elf, Elf) -> (Elf, Elf)
newRecipes (elf1, elf2) = ((moveElf . append newRecipes) elf1,
                   (moveElf . append newRecipes) elf2)
  where newRecipes = Seq.fromList $ combineRecipes (_focus elf1) ( _focus elf2)

lastRecipe :: Elf -> Int
lastRecipe e =  recipe (_forward e)
  where recipe (Seq.viewl -> last Seq.:< rest) = last
        recipe (Seq.Empty) = _focus e

numToStr :: [[Int]] -> String
numToStr = concatMap show . concat

finalScores :: Int -> String
finalScores n = take 10 $ drop (n-2) $ numToStr . map (uncurry combineRecipes . (\(a, b) -> ((_focus a), (_focus b)))) .
  take (n + 10) $ iterate newRecipes (initialA, initialB)

recipes :: Int -> String
recipes n = take n . numToStr . map (uncurry combineRecipes . (\(a, b) -> ((_focus a), (_focus b))))
  $ iterate newRecipes (initialA, initialB)

recipesBeforeScore :: String -> (Elf, Elf) -> Int
recipesBeforeScore target (e1, e2) = go (Seq.empty) (e1, e2) 0
  where new e1' e2' = numToStr . (:[]) $ combineRecipes (_focus e1') ( _focus e2')
        movedElves e1' e2' = newRecipes (e1', e2')
        go curr (x, y) count
          | Seq.fromList target == curr = count + 1
          | otherwise = go
            (Seq.drop (min (length (new x y)) (Seq.length curr - 3)) (curr >< Seq.fromList (new x y)))
            (movedElves x y) $
            (count + 1)
