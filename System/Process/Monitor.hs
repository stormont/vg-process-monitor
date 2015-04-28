
module System.Process.Monitor
   where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans (liftIO)
import Data.Time.Clock
import System.Exit
import System.Process


-- TODO Settings for stdout/stdin/stderr?

data Executable = Executable
   { execCmd  :: String
   , execArgs :: [String]
   } deriving (Show,Read)


data Job = Job
   { jobWorker    :: Executable
   , jobLog       :: Maybe Executable
   , jobData      :: Maybe Executable
   } deriving (Show,Read)


mkJob worker = Job worker Nothing Nothing


-- Intervals at which to run log/data backups, in seconds
data Intervals = Intervals
   { lastLog      :: UTCTime
   , lastData     :: UTCTime
   , logInterval  :: NominalDiffTime
   , dataInterval :: NominalDiffTime
   }


data Comm = Comm
  { commLastStart :: UTCTime
  , commNumFailures :: Int
  , commTerminate :: Bool
  } deriving (Show,Read)


mkComm :: UTCTime -> Comm
mkComm time = Comm time 0 False


launchWorker :: Job
             -> TVar Comm
             -> Intervals
             -> IO ()
launchWorker job comm intervals = do
   h <- startWorker $ jobWorker job
   monitorWorker job comm intervals h


startWorker :: Executable
            -> IO (ProcessHandle)
startWorker worker = do
   (_, _, _, h) <- createProcess (proc (execCmd worker) (execArgs worker))
                                 { std_out = Inherit
                                 , std_in  = Inherit
                                 , std_err = Inherit
                                 }
   return h


reportResults :: TVar Comm
              -> IO ()
reportResults comm = do
   comm' <- liftIO $ atomically $ readTVar comm
   putStrLn $ "Last Time: " ++ show (commLastStart comm')
   putStrLn $ "Failures: " ++ show (commNumFailures comm')


monitorWorker :: Job
              -> TVar Comm
              -> Intervals
              -> ProcessHandle
              -> IO ()
monitorWorker job comm intervals workerHandle = do
   threadDelay 100000  -- 100 ms delay
   exitCode <- getProcessExitCode workerHandle
   case exitCode of
      Nothing -> do
         intervals'  <- checkDataInterval job intervals
         intervals'' <- checkLogInterval  job intervals'
         comm' <- liftIO $ atomically $ readTVar comm
         if commTerminate comm'
            then do
               putStrLn "Terminating..."
               terminateProcess workerHandle
               reportResults comm
            else monitorWorker job comm intervals'' workerHandle
      Just ExitSuccess     -> do
         runDataWorker job
         runLogWorker job
      Just (ExitFailure c) -> do
         performRecovery job
         comm' <- liftIO $ atomically $ readTVar comm
         if commTerminate comm'
            then do
               putStrLn "Terminating..."
               reportResults comm
            else do
               putStrLn "Relaunching worker..."
               curTime <- getCurrentTime
               putStrLn $ "Time: " ++ show curTime
               putStrLn $ "NumFailures: " ++ show ((commNumFailures comm') + 1)
               liftIO $ atomically $ modifyTVar comm (\x -> x { commLastStart = curTime, commNumFailures = (commNumFailures x) + 1 })
               launchWorker job comm (intervals { lastLog = curTime, lastData = curTime })


checkDataInterval :: Job
                  -> Intervals
                  -> IO (Intervals)
checkDataInterval job intervals = do
   curTime <- getCurrentTime
   let diffTime = diffUTCTime curTime (lastData intervals)
   intervals' <- if diffTime > (dataInterval intervals)
      then do
         runDataWorker job
         return $ intervals { lastData = curTime }
      else return intervals
   return $ intervals'


checkLogInterval :: Job
                 -> Intervals
                 -> IO (Intervals)
checkLogInterval job intervals = do
   curTime <- getCurrentTime
   let diffTime = diffUTCTime curTime (lastLog intervals)
   intervals' <- if diffTime > (logInterval intervals)
      then do
         runLogWorker job
         return $ intervals { lastLog = curTime }
      else return intervals
   return $ intervals'


-- Helper/shorthand functions for runWorker
runDataWorker = runWorker jobData "Data worker fired"
runLogWorker  = runWorker jobLog  "Log worker fired"


runWorker :: (Job -> Maybe Executable)
          -> String
          -> Job
          -> IO ()
runWorker extractFunc desc job = do
   case extractFunc job of
      Nothing -> return ()
      Just j  -> do
         _ <- createProcess (proc (execCmd j) (execArgs j))
                            { std_out = Inherit
                            , std_in  = Inherit
                            , std_err = Inherit
                            }
         putStrLn desc


performRecovery :: Job
                -> IO ()
performRecovery job = do
   putStrLn "Recovering data..."
   runDataWorker job
   runLogWorker job
