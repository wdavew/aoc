module Day2 where
import Data.List
import Data.Maybe
import Data.Char

type CharFreqs = [Int]

wordFreqs :: String -> CharFreqs
wordFreqs str = [ freq | char <- (nub str),
                                 let freq = length (filter (== char) str) ]

countByReccurence :: Int -> [CharFreqs] -> Int
countByReccurence num freqs = length $ filter (isJust . (find (== num))) freqs

allPairs :: [String] -> [(String, String)]
allPairs list = [ (i, j) | i <- list, j <- list, i /= j ]

zipIds :: (String, String) -> String
zipIds (a, b) = catMaybes $ zipWith zipper a b
  where zipper c1 c2
          | c1 /= c2 = Nothing
          | otherwise = Just c1
