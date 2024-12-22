module Day05 (solveDay05) where

import Data.List (sortBy)
import Utils (readFileLines)

solveDay05 :: FilePath -> IO (Int, Int)
solveDay05 filePath = do
  input <- readFileLines filePath
  let resultP1 = solveDay05p1 input
      resultP2 = solveDay05p2 input
  return (resultP1, resultP2)

solveDay05p1 :: [String] -> Int
solveDay05p1 input =
    let (rules, pages) = parseInput (unlines input)
        middle p = p !! (length p `div` 2)
        result = map middle $ filter (\p -> not $ any (\r -> r `violatesRule` p) rules) pages
    in sum result

solveDay05p2 :: [String] -> Int
solveDay05p2 input = 
    let (rules, pages) = parseInput (unlines input)
        middle p = p !! (length p `div` 2)
        result = map (middle . createOrdering rules) $ filter (\p -> any (\r -> r `violatesRule` p) rules) pages
    in sum result

type Rule = (Int, Int)
type Page = [Int]

parseRule :: String -> Rule
parseRule s = let (a,_:b) = span (/='|') s in (read a, read b)

parsePage :: String -> [Int]
parsePage = map read . words . map (\c -> if c==',' then ' ' else c)

parseInput :: String -> ([Rule], [Page])
parseInput = (\(r,_:p) -> (map parseRule r, map parsePage p)) . span (/="") . lines

violatesRule :: Rule -> Page -> Bool
violatesRule (a,b) p = a `elem` p && b `elem` p && idx a p > idx b p
  where idx n = head . map fst . filter ((==n) . snd) . zip [0..]

createOrdering :: [Rule] -> [Int] -> [Int]
createOrdering rules = sortBy (\x y -> 
   if any ((==) (x,y) . \(a,b) -> (a,b)) rules then LT
   else if any ((==) (y,x) . \(a,b) -> (a,b)) rules then GT
   else compare x y)
