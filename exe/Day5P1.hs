import Day5

main :: IO ()
main = do
  text <- readFile "./input/day5p1.txt"
  putStr $
    show $
    length .
    fullyReducePolymer .
    map charToUnit $
    filter (/= '\n')
    text
