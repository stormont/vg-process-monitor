module Main
   where

import Control.Concurrent (threadDelay)
import System.Exit


main = do
   putStrLn "Failing in 5..."
   threadDelay 1000000
   putStrLn "Failing in 4..."
   threadDelay 1000000
   putStrLn "Failing in 3..."
   threadDelay 1000000
   putStrLn "Failing in 2..."
   threadDelay 1000000
   putStrLn "Failing in 1..."
   threadDelay 1000000
   putStrLn "Failing"
   exitFailure
