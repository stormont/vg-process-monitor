
module Main
   where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock

import System.Process.Monitor


main = do
   time <- getCurrentTime
   let worker = Executable (B.pack "temp.exe") []
       job = Job worker Nothing Nothing Nothing Nothing
       intervals = Intervals time time 3 1
   putStrLn "Monitoring beginning..."
   launchWorker job intervals
   putStrLn "Monitoring complete"
