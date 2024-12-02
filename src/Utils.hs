module Utils
  ( readFileLines,
    isOrdered,
  )
where

readFileLines :: FilePath -> IO [String]
readFileLines filePath = do
  contents <- readFile filePath
  return $ lines contents

isOrdered :: [Int] -> Bool
isOrdered [] = True
isOrdered [_] = True
isOrdered (x:y:xs)
  | y > x = all (\(a, b) -> b > a) (zip (x:y:xs) (y:xs))
  | y < x = all (\(a, b) -> b < a) (zip (x:y:xs) (y:xs))
  | otherwise = False
