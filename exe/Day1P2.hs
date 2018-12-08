import Day1
import Data.Either
import Data.List

fileName :: String
fileName = "./input/day1p2.txt"

main :: IO ()
main = do
  text <- readFile fileName
  putStr $
    show $
    (last . lefts) $
    takeWhileOneMore isRight $
    scanl calibrateFreqs (Right initialCalibration) $
    cycle (map processInput (lines text))
