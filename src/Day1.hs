module Day1 where
import Data.Set (Set)
import qualified Data.Set as Set

type Frequency = Int
data FreqCalibration = FreqCalibration (Set Frequency) Frequency

processInput :: String -> Frequency
processInput (symbol:num)
  | symbol == '+' = read num
  | otherwise = - (read num)

initialCalibration :: FreqCalibration
initialCalibration = FreqCalibration (Set.singleton 0) 0

calibrateFreqs :: Either Frequency FreqCalibration -> Frequency -> Either Frequency FreqCalibration
calibrateFreqs (Left x) _ = Left x
calibrateFreqs (Right (FreqCalibration freqSet currentFreq)) nextFreq
  | (Set.member newFreq freqSet) = Left newFreq
  | otherwise = Right $ FreqCalibration (Set.insert newFreq freqSet) newFreq
  where newFreq = currentFreq + nextFreq

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x xs -> if p x then x:xs else [x]) []
