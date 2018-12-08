import Day6
import Data.Maybe

main :: IO ()
main = do
  text <- readFile "./input/day6p1.txt"
  putStrLn $
    show $ largestArea $ points text
    where
      points x = strToCoords x
