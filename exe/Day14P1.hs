import Day14

main :: IO ()
main = do
  putStrLn $
    show $ recipesBeforeScore "540391" (initialA, initialB)
