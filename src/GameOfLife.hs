module GameOfLife where

import System.Process (system)
import System.Random (mkStdGen, randomRs)
import System.Console.Terminal.Size (size, Window(..))

type Cell = Bool
type Grid = [[Cell]] -- row major

-- TODO: Check how 'replicate' works
emptyGrid :: Int -> Int -> Grid 
emptyGrid 0 _ = []
emptyGrid y x = emptyRow x : emptyGrid (y-1) x where
    emptyRow 0 = [] 
    emptyRow x = False : emptyRow (x-1)

neighbors :: [(Int, Int)]
neighbors = [ (di, dj) | di <- [-1..1], dj <- [-1..1], (di, dj) /= (0, 0) ]


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


runGame :: IO ()
runGame = do
  mWin <- size
  let seed = 10
  let (screen_height, screen_width) =
        case mWin of
          Just (Window h w) -> (h, w)
          Nothing           -> (24, 80)  

  clearScreen
  printGrid (seededGrid seed screen_height screen_width )
