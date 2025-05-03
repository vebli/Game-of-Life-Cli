module GameOfLife where

import System.Console.Terminal.Size (Window (..), size)
import System.Process (system)
import System.Random (mkStdGen, randomRs)
import Control.Concurrent (threadDelay)

type Cell = Bool

type Grid = [[Cell]] -- row major

-- TODO: Check how 'replicate' works
-- emptyGrid :: Int -> Int -> Grid
-- emptyGrid 0 _ = []
-- emptyGrid y x = emptyRow x : emptyGrid (y - 1) x
--   where
--     emptyRow 0 = []
--     emptyRow x = False : emptyRow (x - 1)

neighbors :: Int -> Int -> Int -> Int -> [(Int, Int)]
neighbors col row col_size row_size= [(col + dc, row + dr) | (dc, dr) <- offsets,
  let offsetCol = col + dc,
  let offsetRow = row + dr,
  offsetCol >= 0,
  offsetRow >= 0,
  offsetCol < col_size,
  offsetRow < row_size
  ]
  where
    offsets = [(di, dj) | di <- [-1 .. 1], dj <- [-1 .. 1], (di, dj) /= (0, 0)]

countNeighbors :: Grid -> Int -> Int -> Int -> Int -> Int
countNeighbors grid col row col_size row_size= length $ filter (\(c, r) ->  (grid !! r !! c) == True) (neighbors col row col_size row_size)

nextCell :: Cell -> Int -> Cell 
nextCell True num_neighbors 
  | num_neighbors == 2 || num_neighbors == 3 = True
  | otherwise = False
nextCell False num_neighbors 
  | num_neighbors == 3 = True
  | otherwise = False

-- updateGrid:: Grid -> Int -> Int -> Grid
-- updateGrid grid 0 row = [] 
-- updateGrid (r:grid) col row = updateRow r : (updateGrid grid col-1 row)
--   where
--     updateRow r = map (countNeighbors grid col row col_size row_size) (map nextCell r)
--

updateGrid :: Grid -> Grid
updateGrid grid =
  let rowSize = length grid
      colSize = if null grid then 0 else length (head grid)
  in
    [ [ nextCell (grid !! y !! x) (countNeighbors grid x y colSize rowSize)
      | x <- [0..colSize-1] ]
    | y <- [0..rowSize-1] ]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

seededGrid :: Int -> Int -> Int -> Grid
seededGrid seed rows cols = chunk cols limitedCells
  where
    gen = mkStdGen seed
    bits = randomRs (0, 1) gen :: [Int]
    total = rows * cols
    limitedCells = take total $ map (== 1) bits

clearScreen :: IO ()
clearScreen = do
  _ <- system "clear"
  return ()

printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . map (\c -> if c then '█' else ' '))

runGameLoop :: Grid -> IO ()
runGameLoop grid = do
  clearScreen
  printGrid grid
  threadDelay 100000 
  let newGrid = updateGrid grid
  runGameLoop newGrid

runGame :: IO ()
runGame = do
  mWin <- size
  let seed = 10
  let (screen_height, screen_width) =
        case mWin of
          Just (Window h w) -> (h, w)
          Nothing -> (100, 100)
  let grid = seededGrid seed screen_height screen_width
  runGameLoop grid
