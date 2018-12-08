import Data.Maybe
import Day3

main :: IO ()
main = do
  text <- readFile "./input/day3p2.txt"
  putStrLn $
    toStr $ findLoneClaim (claims text)
    where claims x = catMaybes (map readClaim $ lines x)
          toStr (Just claim) = show $ Day3.id claim
          toStr Nothing = "No isolated claim found."
