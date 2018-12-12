import Day10

main :: IO ()
main = do
  txt <- readFile "./input/Day10p2.txt"
  putStrLn $
    show $ length .
    takeWhile (\ x -> not . all (hasNeighbor 3 (map fst x)) $ map fst x) .
    iterate (map evolve) $ map stringToCoords (lines txt)
