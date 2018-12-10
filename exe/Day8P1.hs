import Day8

main :: IO ()
main = do
  text <- readFile "./input/day8p1.txt"
  putStrLn $
    show $ sumLicenses $ readLicense text
