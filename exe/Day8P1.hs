import Day8
import Data.List

main :: IO ()
main = do
  text <- readFile "./input/day8p1.txt"
  putStrLn $
    show $
    sumLicenses . readLicense $
    (flip (++) " ") $ intercalate " " $ lines text
