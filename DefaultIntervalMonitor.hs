
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
   doWork args comm time
   putStrLn "Monitoring complete"


doWork args comm time = do
   let workerBin = head args
       intervalBin = head $ drop 1 args
       worker = Executable workerBin []
       job = mkJob worker
   launchWorker job
                comm
                [ Interval time 3 intervalJob1
                , Interval time 5 intervalJob2
                ]


intervalJob1 _ =
   putStrLn "Example interval 1"


intervalJob2 _ =
   putStrLn "Example interval 2"
