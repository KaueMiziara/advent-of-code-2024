module Day03 (solveDay03) where

import Data.Char (isDigit)
import Utils (readFileLines)

solveDay03 :: FilePath -> IO (Int, Int)
solveDay03 filePath = do
  input <- readFileLines filePath
  let resultP1 = solveDay03p1 input
      resultP2 = solveDay03p2 input
  return (resultP1, resultP2)

solveDay03p1 :: [String] -> Int
solveDay03p1 input = sum $ map (sum . map multiply . extractMul) input

solveDay03p2 :: [String] -> Int
solveDay03p2 = fst . foldl processHelper (0, True)
  where
    processHelper (totalSum, currentState) line =
      let (lineSum, newState) = handleState line currentState
       in (totalSum + lineSum, newState)


multiply :: (Int, Int) -> Int
multiply (x, y) = x * y

extractMul :: String -> [(Int, Int)]
extractMul [] = []
extractMul ('m' : 'u' : 'l' : '(' : rest) =
  case span isDigit rest of
    (num1, ',' : rest') ->
      case span isDigit rest' of
        (num2, ')' : rest'') ->
          (read num1, read num2) : extractMul rest''
        _ -> extractMul rest'
    _ -> extractMul rest
extractMul (_ : rest) = extractMul rest

handleState :: String -> Bool -> (Int, Bool)
handleState [] state = (0, state)
handleState ('d' : 'o' : '(' : ')' : rest) _ = handleState rest True
handleState ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : rest) _ = handleState rest False
handleState ('m' : 'u' : 'l' : '(' : rest) True =
  case span isDigit rest of
    (num1, ',' : rest') ->
      case span isDigit rest' of
        (num2, ')' : rest'') ->
          let (sumRest, finalState) = handleState rest'' True
           in (read num1 * read num2 + sumRest, finalState)
        _ -> handleState rest' True
    _ -> handleState rest True
handleState ('m' : 'u' : 'l' : '(' : rest) False =
  handleState (dropWhile (/= ')') rest) False
handleState (_ : rest) state = handleState rest state
