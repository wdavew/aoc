import Day7

main :: IO ()
main = do
  text <- readFile "./input/day7p1.txt"
  putStrLn $
    show $
    stepSequence $ connectGraphs $ (map pairToDag) . (map lineToConstraint) $ (lines text)
