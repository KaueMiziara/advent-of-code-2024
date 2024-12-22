module Main (main) where

import Day05

main :: IO ()
main = do
  -- (p1, p2) <- solveDay05 "src/itest.txt"
  (p1, p2) <- solveDay05 "input/day05.txt"
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
