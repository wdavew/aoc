module Day3 where

import Data.Char
import Text.ParserCombinators.ReadP

-- Claim Parser
digit :: ReadP Char
digit = satisfy (\x -> x >= '0' && x <= '9')

numbers :: ReadP Int
numbers = do
  numbers <- many1 digit
  return (read numbers)

parseId :: ReadP Int
parseId = do
  char '#'
  id <- numbers
  string " @ "
  return (id)

parsePosition :: ReadP (Int, Int)
parsePosition = do
  x <- numbers
  char ','
  y <- numbers
  string ": "
  return (x, y)

parseDim :: ReadP (Int, Int)
parseDim = do
  w <- numbers
  char 'x'
  h <- numbers
  eof
  return (w, h)

data Claim = Claim { id :: Int
                   , x :: Int
                   , y :: Int
                   , width :: Int
                   , height :: Int
                   }
             deriving Show


parseClaim :: ReadP Claim
parseClaim = do
  id <- parseId
  pos <- parsePosition
  dim <- parseDim
  return (Claim id (fst pos) (snd pos) (fst dim) (snd dim))

readClaim :: String -> Maybe Claim
readClaim s = get $ readP_to_S parseClaim s
  where get [(c, _)] = Just c
        get _ = Nothing

-- Library
type Point = (Int, Int)
type Fabric = [Point]
data Rectangle = Rectangle Point Int Int
  deriving Show

fabric :: Fabric
fabric = [ (x, y) | x <- [0..1000], y <- [0..1000]]

intersect :: Point -> Rectangle -> Bool
intersect (x, y) (Rectangle (x', y') width height) =
  x' <= x &&
  x <= x'+ width - 1 &&
  y' - height + 1 <= y &&
  y <= y'

intersectRect :: Rectangle -> Rectangle -> Bool
intersectRect
  (Rectangle (x, y) width height)
  (Rectangle (x', y') width' height') =
  x' <= x &&
  x <= x'+ width - 1 &&
  y' - height + 1 <= y &&
  y <= y'

claimToRect :: Claim -> Rectangle
claimToRect claim = Rectangle (x claim, 1000 - y claim) (width claim) (height claim)

rectToFabric :: Rectangle -> Fabric
rectToFabric (Rectangle (x, y) w h) = [ (i, j) | i <- [x..(x + w - 1)], j <- [(y - h + 1)..y] ]

claimToFabric = rectToFabric . claimToRect

claimsOnPoint :: [Claim] -> Point -> Int
claimsOnPoint claims point = length . filter (intersect point) . (map claimToRect) $ claims

doubleClaimed :: [Claim] -> Point -> Bool
doubleClaimed claims = (> 1) . claimsOnPoint claims

countIntersections :: [Claim] -> Fabric -> Int
countIntersections claims = length . (filter $ doubleClaimed claims)

findLoneClaim :: [Claim] -> Maybe Claim
findLoneClaim allClaims = safeHead $ filter ((== 0). (countIntersections allClaims) . claimToFabric) allClaims
  where safeHead (x:[]) = Just x
        safeHead _ = Nothing
