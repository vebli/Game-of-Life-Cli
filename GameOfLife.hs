module GameOfLife where

import System.Process (system)

clearScreen :: IO()
clearScreen = do 
    _ <- system "clear"
    return ()

runGame :: IO()
runGame = do
    clearScreen
    runGame
