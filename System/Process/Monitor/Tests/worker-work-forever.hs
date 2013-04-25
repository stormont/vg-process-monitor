module Main
   where

import Control.Concurrent (threadDelay)


main = do
   doWork


doWork = do
   putStrLn "working..."
   threadDelay 1000000
   doWork
