import Day5

main :: IO ()
main = do
  text <- readFile "./input/day5p2.txt"
  putStrLn $
    show $
    shortestPolymer .
    map charToUnit $
    filter (/= '\n')
    text
