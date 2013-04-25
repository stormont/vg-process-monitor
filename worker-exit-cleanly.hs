module Main
   where

import Control.Concurrent (threadDelay)
import System.Exit


main = do
   putStrLn "Exiting in 5..."
   threadDelay 1000000
   putStrLn "Exiting in 4..."
   threadDelay 1000000
   putStrLn "Exiting in 3..."
   threadDelay 1000000
   putStrLn "Exiting in 2..."
   threadDelay 1000000
   putStrLn "Exiting in 1..."
   threadDelay 1000000
   putStrLn "Exiting"
   exitSuccess
