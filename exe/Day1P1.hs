import Day1

fileName :: String
fileName = "./input/day1p1.txt"

main :: IO ()
main = do
  text <- readFile fileName
  putStr $ show (sum $ map processInput $ lines text) ++ "\n"
