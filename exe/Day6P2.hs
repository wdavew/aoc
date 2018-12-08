import Day6
import Data.Maybe

main :: IO ()
main = do
  text <- readFile "./input/day6p2.txt"
  putStrLn $
    show $ regionSize 10000 $ points text
    where
      points x = strToCoords x
