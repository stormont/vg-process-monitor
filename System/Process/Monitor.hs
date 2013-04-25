
module System.Process.Monitor
   where

import qualified Data.ByteString.Lazy.Char8 as B
import Control.Concurrent (threadDelay)
import Data.Time.Clock
import System.Exit
import System.Process


-- TODO Settings for stdout/stdin/stderr?

data Executable = Executable
   { execCmd  :: B.ByteString
   , execArgs :: [B.ByteString]
   } deriving (Show,Read)


data Job = Job
   { jobWorker    :: Executable
   , jobLog       :: Maybe Executable
   , jobData      :: Maybe Executable
   , jobCleanStop :: Maybe Executable
   , jobUpgrade   :: Maybe Executable
   } deriving (Show,Read)


-- Intervals at which to run log/data backups, in seconds
data Intervals = Intervals
   { lastLog      :: UTCTime
   , lastData     :: UTCTime
   , logInterval  :: NominalDiffTime
   , dataInterval :: NominalDiffTime
   }


launchWorker :: Job
             -> Intervals
             -> IO ()
launchWorker job intervals = do
   h <- startWorker $ jobWorker job
   monitorWorker job h intervals


startWorker :: Executable
            -> IO (ProcessHandle)
startWorker worker = do
   (_, _, _, h) <- createProcess (proc (B.unpack $ execCmd worker) (map (B.unpack) $ execArgs worker))
                                 { std_out = Inherit
                                 , std_in  = Inherit
                                 , std_err = Inherit
                                 }
   return h


monitorWorker :: Job
              -> ProcessHandle
              -> Intervals
              -> IO ()
monitorWorker job workerHandle intervals = do
   threadDelay 100000  -- 100 ms delay
   exitCode <- getProcessExitCode workerHandle
   case exitCode of
      Nothing -> do
         intervals'  <- checkDataInterval job intervals
         intervals'' <- checkLogInterval  job intervals'
         monitorWorker job workerHandle intervals''
      Just ExitSuccess     -> do
         runDataWorker job
         runLogWorker job
      Just (ExitFailure c) -> do
         performRecovery job
         putStrLn "Relaunching worker..."
         curTime <- getCurrentTime
         launchWorker job (intervals { lastLog = curTime, lastData = curTime })


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
         _ <- createProcess (proc (B.unpack $ execCmd j) (map (B.unpack) $ execArgs j))
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
