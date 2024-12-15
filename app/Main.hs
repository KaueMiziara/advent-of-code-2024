module Main (main) where

import Day04

main :: IO ()
main = do
  (p1, p2) <- solveDay04 "src/itest.txt"
  -- (p1, p2) <- solveDay03 "input/day04.txt"
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
