import Day2

main :: IO ()
main = do
  text <- readFile "./input/day2p1.txt"
  putStrLn $
    show $
    (twos $ freqs text) * (threes $ freqs text)
    where freqs x = map wordFreqs (lines x)
          twos = countByReccurence 2
          threes = countByReccurence 3
