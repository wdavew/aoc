import Data.Maybe
import Day3

main :: IO ()
main = do
  text <- readFile "./input/day3p1.txt"
  putStrLn $
    show $
    countIntersections (claims text) fabric
    where claims x = catMaybes (map readClaim $ lines x)
