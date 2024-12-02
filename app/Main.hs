module Main (main) where

import Day02

main :: IO ()
main = do
  -- (p1, p2) <- solveDay02 "src/day02test.txt"
  (p1, p2) <- solveDay02 "input/day02.txt"
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
