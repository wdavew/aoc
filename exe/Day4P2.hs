import Day4

main :: IO ()
main = do
  text <- readFile "./input/day4p2.txt"
  putStrLn $
    "Id: " ++ (show $ id text) ++ " Minute: " ++ (show $ m text)
        where
        log = defaultToEmpty . readLog . sortInput
        id text = mostHabitualGuard (log text)
        m text = sleepiestMinute (id text) (log text)
