import Day15
import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import Debug.Trace

runGame :: GameState -> IO ()
runGame gs = forever $ do
  putStrLn $(show $ _complete gs)
  putStrLn $(show $ _rounds gs)
  putStrLn $ show $ length . M.keys . M.filter ((== Elf) . _faction) $ (_units gs)
  putStrLn $ printBoard gs
  threadDelay 1000000
  runGame (nextRound . resolveRound $ gs)

-- too tired / late to do binary search with this, just going to brute force it while I take a break
findElfAP :: Int -> GameState -> Int
findElfAP ap gs
  | finalElves == initialElves = ap
  | otherwise = trace (show finalElves ++ " " ++ show initialElves) $ findElfAP (ap + 1) gs
  where modifiedStart = gs { _units = M.map (\u -> if (_faction u ) == Elf then u { _ap = ap } else u) (_units gs) }
        finalState = head . dropWhile (not . _complete) $ (iterate (nextRound . resolveRound) modifiedStart)
        finalElves = length . M.keys . M.filter ((== Elf) . _faction) $ (_units finalState)
        initialElves = length . M.keys . M.filter ((== Elf) . _faction) $ (_units gs)

main :: IO ()
main =  do
  t <- readFile "./input/day15.txt"
  putStrLn $ show $ (\(a, b) -> (a * b)) . summarizeGame . head . dropWhile (not . _complete) $ (iterate (nextRound . resolveRound) (moreAp $ initState t))
  putStrLn $ show $ findElfAP 14 (initState t)
  where initState t = (nextTurn (linesToBoard emptyState (lines t)))
        moreAp state = state { _units = M.map (\u -> if (_faction u ) == Elf then u { _ap = 15 } else u) (_units state) }
