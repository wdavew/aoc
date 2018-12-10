import Day8
import Data.List

main :: IO ()
main = do
  text <- readFile "./input/day8p2.txt"
  putStrLn $
    show $
    treeValue . readLicense $
    (flip (++) " ") $ intercalate " " $ lines text
