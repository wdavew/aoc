import Day7

main :: IO ()
main = do
  text <- readFile "./input/day7p2.txt"
  putStrLn $
    show $
    totalTime (buildSched 5) $
    connectGraphs $ (map pairToDag) . (map lineToConstraint) $ (lines text)
