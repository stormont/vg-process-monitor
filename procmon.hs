
module Main
   where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock
import System.Environment (getArgs)

import System.Process.Monitor


main = do
   args <- getArgs
   time <- getCurrentTime
   let worker = Executable (B.pack $ head args) []
       job = Job worker Nothing Nothing Nothing Nothing
       intervals = Intervals time time 3 1
   putStrLn "Monitoring beginning..."
   launchWorker job intervals
   putStrLn "Monitoring complete"
