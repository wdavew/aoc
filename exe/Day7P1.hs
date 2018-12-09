import Day7

main :: IO ()
main = do
  text <- readFile "./input/day7p2.txt"
  putStrLn $
    show $
    stepSequence $ connectGraphs $ (map pairToDag) . (map lineToConstraint) $ (lines text)
