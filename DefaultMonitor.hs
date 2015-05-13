
module Main
   where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans (liftIO)
import Data.Time
import System.Environment (getArgs)

import System.Process.Monitor


main = do
   args <- getArgs
   putStrLn "Monitoring beginning..."
   time <- getCurrentTime
   comm <- liftIO $ atomically $ newTVar $ mkComm time
   doWork args comm
   putStrLn "Monitoring complete"


doWork args comm = do
   let worker = Executable (head args) []
       job = mkJob worker
   launchWorker job comm []
