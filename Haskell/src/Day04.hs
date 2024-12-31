module Day04 (solveDay04) where

import Utils (readFileLines)

solveDay04 :: FilePath -> IO (Int, Int)
solveDay04 filePath = do
  input <- readFileLines filePath
  let resultP1 = solveDay04p1 input
      resultP2 = solveDay04p2 input
  return (resultP1, resultP2)

solveDay04p1 :: [String] -> Int
solveDay04p1 grid = length $ findAllOccurrences grid "XMAS"

solveDay04p2 :: [String] -> Int
solveDay04p2 input = countAllXmasOccurrences (toGrid input)

findAllOccurrences :: [String] -> String -> [(Int, Int, (Int, Int))]
findAllOccurrences grid word = 
  [(x, y, dir) | x <- [0 .. rows - 1], y <- [0 .. cols - 1], dir <- directions, match grid word x y dir]
  where
    rows = length grid
    cols = length (head grid)
    directions = [(1, 0), (0, 1), (1, 1), (1, -1), (-1, 0), (0, -1), (-1, -1), (-1, 1)]

match :: [String] -> String -> Int -> Int -> (Int, Int) -> Bool
match grid word x y (dx, dy) =
  all (\(i, c) -> inBounds (x + i * dx) (y + i * dy) && grid !! (x + i * dx) !! (y + i * dy) == c) indexedWord
  where
    indexedWord = zip [0 ..] word
    rows = length grid
    cols = length (head grid)
    inBounds i j = i >= 0 && i < rows && j >= 0 && j < cols


type Coord = (Int, Int)
type Grid = [(Coord, Char)]

countAllXmasOccurrences :: Grid -> Int
countAllXmasOccurrences grid =
  length $ filter (isXmas grid) (findAPositions grid)

isXmas :: Grid -> Coord -> Bool
isXmas grid (x, y) = thisCombo `elem` validCombos
  where
    lookupFn c = lookup c grid
    tl = fromMaybe ' ' (lookupFn (x - 1, y - 1))
    tr = fromMaybe ' ' (lookupFn (x + 1, y - 1))
    bl = fromMaybe ' ' (lookupFn (x - 1, y + 1))
    br = fromMaybe ' ' (lookupFn (x + 1, y + 1))
    thisCombo =
      [ [tl, ' ', tr],
        [' ', 'A', ' '],
        [bl, ' ', br]
      ]
    validCombos =
      [ [ "M S",
          " A ",
          "M S"
        ],
        [ "M M",
          " A ",
          "S S"
        ],
        [ "S M",
          " A ",
          "S M"
        ],
        [ "S S",
          " A ",
          "M M"
        ]
      ]

findAPositions :: Grid -> [Coord]
findAPositions = map fst . filter (\(_, char) -> char == 'A')

toGrid :: [String] -> Grid
toGrid grid =
  [ ((i, j), char)
    | (j, row) <- zip [0 ..] grid,
      (i, char) <- zip [0 ..] row,
      char `elem` "MAS"
  ]

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

