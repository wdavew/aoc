{-# LANGUAGE OverloadedStrings #-}
module Day7 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph (topSort)
import Data.Graph
import qualified Data.Set as Set
import Data.List
import Text.ParserCombinators.ReadP

-- Parser
parseConstraint :: ReadP (Char, Char)
parseConstraint = do
  string "Step "
  c1 <- (satisfy (\x -> x >= 'A' && x <= 'Z'))
  string " must be finished before step "
  c2 <- (satisfy (\x -> x >= 'A' && x <= 'Z'))
  return (c1, c2)
lineToConstraint :: String -> (Char, Char)
lineToConstraint s = (fst . head) $ readP_to_S parseConstraint s

-- Part 1
class AlgGraph a where
  empty :: a
  fromChar :: Char -> a
  overlay :: a -> a -> a
  connect :: a -> a -> a

data StepGraph = StepGraph (Map Char String)

instance AlgGraph StepGraph where
  empty = StepGraph Map.empty
  fromChar v = StepGraph (Map.singleton v [])
  overlay (StepGraph map1) (StepGraph map2) = StepGraph (Map.unionWith (++) map1 map2)
  connect (StepGraph map1) (StepGraph map2) = StepGraph ((Map.unionWith (++) map2) . (addEdges map2) $ map1)
    where addEdges depMap priorMap = fmap (++ (Map.keys depMap)) priorMap

instance Show StepGraph where
  show (StepGraph map) = show map

pairToDag :: (Char, Char) -> StepGraph
pairToDag (c1, c2) = connect (fromChar c1) (fromChar c2)

connectGraphs :: [StepGraph] -> StepGraph
connectGraphs = foldr overlay (empty :: StepGraph)

available :: StepGraph -> Char -> Bool
available (StepGraph am) chr = not $ elem chr (concat (Map.elems am))

stepSequence :: StepGraph -> String
stepSequence (StepGraph map)
  | map == Map.empty = ""
  | otherwise = [firstAvailable] ++ stepSequence subGraph
  where firstAvailable = (head . sort) . filter (available (StepGraph map)) $ (Map.keys map)
        subGraph = (StepGraph $ Map.delete firstAvailable map)

-- Part 2
data Worker = Worker { task :: Char, time :: Int }
  deriving Show

type Schedule = Map Int Worker

cost :: Map Char Int
cost = Map.fromList $ zip ['A'..'Z'] [61..]
getCost = flip (Map.findWithDefault 0) cost

removeConstraint :: Char -> StepGraph -> StepGraph
removeConstraint char (StepGraph m) = StepGraph (Map.delete char m)

buildSched :: Int -> Schedule
buildSched x = Map.fromList $ [(i, (Worker '_' 0)) | i <- [0..(x-1)]]

idleWorkers :: Schedule -> [Int]
idleWorkers sched = Map.keys (Map.filter ((== 0) . time) sched)

allIdle :: Schedule -> Bool
allIdle x = (== length x) . (length . idleWorkers) $ x

scheduleTask :: Char -> Schedule -> Schedule
scheduleTask task sched = assignTask (idleWorkers sched)
  where assignTask [] = sched
        assignTask (x:_) = Map.insert x (Worker task (getCost task)) sched

takenTasks :: Schedule -> [Char]
takenTasks m = Map.foldr (:) [] takenTasks
  where takenTasks = fmap task (Map.withoutKeys m (Set.fromList (idleWorkers m)))

completedTasks :: Schedule -> [Char]
completedTasks m = Map.foldr (:) [] completedTasks
  where completedTasks = fmap task (Map.restrictKeys m (Set.fromList (idleWorkers m)))

tickWorker :: Worker -> Worker
tickWorker (Worker { task = tsk, time = t }) = Worker tsk (max (t - 1) 0)

tick :: Schedule -> Schedule
tick = Map.map tickWorker

graphMap (StepGraph m) = m

totalTime :: Schedule -> StepGraph -> Int
totalTime sched graph@(StepGraph gmap)
  | allIdle sched && (length availableTasks == 0) = 0
  | otherwise = 1 + (totalTime updatedSchedule newGraph)
  where newGraph = foldr removeConstraint graph (completedTasks sched)
        availableTasks = sort . filter (not . ((flip elem) (takenTasks sched))) . filter (available newGraph) $ (Map.keys (graphMap newGraph))
        updatedSchedule = (tick . foldr scheduleTask sched) (reverse availableTasks)
