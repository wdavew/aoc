module Day4 where

import Text.ParserCombinators.ReadP as ReadP
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative


type Id = Int
type Interval = (Int, Int)
type Minute = Int
data Sleep = Sleep { interval :: Interval, id :: Id }
  deriving Show
type Log = [Sleep]
data Timestamp = Timestamp { year :: Int
                 , month :: Int
                 , day :: Int
                 , hour :: Int
                 , minute :: Int
                 }
        deriving (Show, Eq)

instance Ord Timestamp where
  compare (Timestamp year month day hour minute) (Timestamp year' month' day' hour' minute') =
    mconcat [ compare year year', compare month month', compare day day', compare hour hour', compare minute minute']


-- Parser
digit :: ReadP Char
digit = satisfy (\x -> x >= '0' && x <= '9')

number :: ReadP Int
number = do
  num <- many1 digit
  return (read num)

timestamp :: ReadP Timestamp
timestamp = do
  char '['
  year <- number
  char '-'
  month <- number
  char '-'
  day <- number
  char ' '
  hour <- number
  char ':'
  minute <- number
  string "] "
  return (Timestamp year month day hour minute)

shiftStart :: ReadP Id
shiftStart = do
  timestamp
  id <- guardId
  return id

guardId :: ReadP Id
guardId = do
  string "Guard #"
  id <- number
  string " begins shift"
  return (id)

parseInterval :: ReadP Interval
parseInterval = do
  t0 <- timestamp
  string "falls asleep\n"
  t1 <- timestamp
  string "wakes up"
  ReadP.optional $ char '\n'
  return ((minute t0), (minute t1))

parseLogUnit :: ReadP Log
parseLogUnit = do
  id <- shiftStart
  char '\n'
  intervals <- ReadP.many parseInterval
  return (map (\x -> (Sleep x id)) intervals)

parseLog :: ReadP Log
parseLog = do
  x <- ReadP.many parseLogUnit
  eof
  return (mconcat x)

safeParse :: (ReadP a) -> String -> Maybe a
safeParse parser x = case (readP_to_S parser x) of
  ((x, _):[]) -> Just x
  _ -> Nothing

readTimestampFromLine = safeParse timestamp
readLog = safeParse parseLog

defaultTo0 :: Maybe Timestamp -> Timestamp
defaultTo0 = fromMaybe (Timestamp 0 0 0 0 0)

defaultToEmpty :: Maybe Log -> Log
defaultToEmpty = fromMaybe []

sortInput :: String -> String
sortInput = (intercalate "\n") . (sortOn (defaultTo0 . readTimestampFromLine)) . lines

-- Part 1
width :: Interval -> Int
width x = snd x - fst x

contains :: Int -> Interval -> Bool
contains x (lower, upper) = lower <= x && x < upper

sleepMinutes :: Log -> Int
sleepMinutes log = sum . (map (width . interval)) $ log

compareSleep :: Log -> Id -> Id -> Ordering
compareSleep log a b = compare (totalSleep a) (totalSleep b)
  where totalSleep x = sleepMinutes (filterLog x log)

shiftsAsleep :: Log -> Minute -> Int
shiftsAsleep log minute = length .
          (filter (contains minute)) .
          (map interval) $ log

compareShiftsAsleep :: Log -> Minute -> Minute -> Ordering
compareShiftsAsleep log a b = compare (shiftsAsleep log a) (shiftsAsleep log b)

filterLog :: Id -> Log -> Log
filterLog id = (filter ((== id) . Day4.id))

sleepiestMinute :: Id -> Log -> Minute
sleepiestMinute id log = maximumBy (compareShiftsAsleep (filterLog id log)) $ [1..59] :: Minute

sleepiestGuard :: Log -> Id
sleepiestGuard log = maximumBy (compareSleep log) (ids log)
  where
    ids x = nub . map Day4.id $ x

-- Part 2
compareByFreq :: Log -> Id -> Id -> Ordering
compareByFreq log a b = compare (highestFreq a) (highestFreq b)
  where highestFreq a = maximum . map (shiftsAsleep (filterLog a log)) $ [1..59]

mostHabitualGuard :: Log -> Id
mostHabitualGuard log = maximumBy (compareByFreq log) $ map Day4.id log
