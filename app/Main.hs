module Main (main) where

import Day03

main :: IO ()
main = do
  -- (p1, p2) <- solveDay03 "src/day03test.txt"
  (p1, p2) <- solveDay03 "input/day03.txt"
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
