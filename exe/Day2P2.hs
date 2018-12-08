import Day2

main :: IO ()
main = do
  text <- readFile "./input/day2p2.txt"
  putStrLn $
    show $
    findId $
    map zipIds $ allPairs (lines text)
    where findId l = head $ filter (\x -> (length x) == 25) l
