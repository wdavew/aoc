module Day8 where
import Text.ParserCombinators.ReadP

type Node = [Int]
data RTree = RTree Node [RTree]
  deriving Show

treeFromList :: [Int] -> [RTree] -> RTree
treeFromList node subTrees = (RTree node subTrees)

number :: ReadP Int
number = do
  x <- many1 $ satisfy (\x -> (x >= '0' && x <= '9'))
  char ' '
  return (read x)

tree :: ReadP RTree
tree = do
  numSubtrees <- number
  numMetadata <- number
  subTrees <- (count numSubtrees) tree
  metadata <- (count numMetadata) number
  return (RTree metadata subTrees)

readLicense :: String -> RTree
readLicense s = (fst . last) (readP_to_S tree s)

foldTree :: ([Int] -> [b] -> b) -> b -> RTree -> b
foldTree f x (RTree node []) = f node [x]
foldTree f x (RTree node rest) = f node (map (foldTree f x) rest)

sumLicenses :: RTree -> Int
sumLicenses = foldTree (\a b -> sum a + sum b) 0
