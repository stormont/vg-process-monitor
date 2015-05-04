
module Main
   where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans (liftIO)
import Data.Time.Clock
import System.Environment (getArgs)

import System.Process.Monitor


main = do
   args <- getArgs
   putStrLn "Monitoring beginning..."
   time <- getCurrentTime
   comm <- liftIO $ atomically $ newTVar $ mkComm time
   doWork args comm time
   putStrLn "Monitoring complete"


doWork args comm time = do
   let worker = Executable (head args) []
       job = mkJob worker
       intervals = [ (mkInterval time) { intervalMinTime = 3 }
                   , mkInterval time ]
   _ <- forkIO $ launchWorker job comm intervals
   return ()
