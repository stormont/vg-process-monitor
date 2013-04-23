
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


data IntervalExecutable = IntervalExecutable
   { executable :: Executable
   , interval   :: Int
   } deriving (Show,Read)


data Job = Job
   { jobWorker    :: Executable
   , jobLog       :: Maybe IntervalExecutable
   , jobData      :: Maybe IntervalExecutable
   , jobCleanStop :: Maybe Executable
   , jobUpgrade   :: Maybe Executable
   } deriving (Show,Read)


-- Intervals at which to run log/data backups, in seconds
data Intervals = Intervals
   { lastTime     :: UTCTime
   , logInterval  :: NominalDiffTime
   , dataInterval :: NominalDiffTime
   }


launchWorker :: Job -> Intervals -> IO ()
launchWorker job intervals = do
   h <- startWorker $ jobWorker job
   monitorWorker job h intervals


startWorker :: Executable -> IO (ProcessHandle)
startWorker worker = do
   (_, _, _, h) <- createProcess (proc (B.unpack $ execCmd worker) (map (B.unpack) $ execArgs worker))
                                 { std_out = CreatePipe
                                 , std_in  = CreatePipe
                                 , std_err = CreatePipe
                                 }
   return h


monitorWorker :: Job -> ProcessHandle -> Intervals -> IO ()
monitorWorker job workerHandle intervals = do
   threadDelay 100000  -- 100 ms delay
   exitCode <- getProcessExitCode workerHandle
   case exitCode of
      Nothing -> do
         runWorkerIfReady job intervals dataInterval runDataWorker
         runWorkerIfReady job intervals logInterval  runLogWorker
         curTime <- getCurrentTime
         monitorWorker job workerHandle (intervals { lastTime = curTime })
      Just ExitSuccess     -> do
         runDataWorker job
         runLogWorker job
      Just (ExitFailure c) -> do
         performRecovery job
         putStrLn "Relaunching worker..."
         curTime <- getCurrentTime
         launchWorker job (intervals { lastTime = curTime })


runWorkerIfReady :: Job -> Intervals -> (Intervals -> NominalDiffTime) -> (Job -> IO ()) -> IO ()
runWorkerIfReady job intervals extractFunc runFunc = do
   curTime <- getCurrentTime
   let diffTime = diffUTCTime curTime (lastTime intervals)
   if diffTime > (extractFunc intervals)
      then runFunc job
      else return ()


runDataWorker :: Job -> IO ()
runDataWorker job = putStrLn "Ran data worker"


runLogWorker :: Job -> IO ()
runLogWorker job = putStrLn "Ran log worker"


performRecovery :: Job -> IO ()
performRecovery job = do
   putStrLn "Recovering data..."
   runDataWorker job
   runLogWorker job
