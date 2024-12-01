module Main (main) where

import Day01

main :: IO ()
main = do
  (d1p1, d1p2) <- solveDay01 "input/day01.txt"
  putStrLn $ "Day 1 Part 1: " ++ show d1p1
  putStrLn $ "Day 2 Part 2: " ++ show d1p2
