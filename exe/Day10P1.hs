import Graphics.Gloss
import Day10
import Data.Word
import qualified Data.ByteString as Byte

size = 501

-- Unncessary, but animating it because it looks cool and keeps with
-- the theme of the question.

main :: IO ()
main = do
  txt <- readFile "./input/Day10p1.txt"
  simulate (InWindow "The Stars Align" (size, size) (0, 0)) white 20 (model 200 txt) (toPic txt) stepIter
    where
          model st text = evolveToMessage st (map stringToCoords $ lines text)
          toPic txt = pic (sampleCoord . coordMap . map fst . model 3 $ txt) 250
          stepIter view t m
            | all (hasNeighbor 3 (map fst m)) (map fst m) = m
            | otherwise = nextFrame m

pic :: (Int, Int) -> Int -> [(Coord, Coord)] -> Picture
pic center padding coords = image $
  bitmapData center padding coords

image :: Byte.ByteString -> Picture
image dta = bitmapOfByteString size size (BitmapFormat TopToBottom PxRGBA) dta True

nextFrame :: [(Coord, Coord)] -> [(Coord, Coord)]
nextFrame = map evolve
