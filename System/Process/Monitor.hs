
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


mkExecutable :: Executable
mkExecutable = Executable "" []


data Job = Job
   { jobWorker :: Executable
   } deriving (Show,Read)


mkJob :: Executable -> Job
mkJob worker = Job worker


data Interval = Interval
   { intervalLastTime :: UTCTime
   , intervalMinTime  :: NominalDiffTime
   , intervalWorker   :: (Job -> IO ())
   }


mkInterval :: UTCTime -> Interval
mkInterval time = Interval time 1 (\_ -> return ())


data Comm = Comm
  { commLastStart :: UTCTime
  , commNumFailures :: Int
  , commTerminate :: Bool
  } deriving (Show,Read)


mkComm :: UTCTime -> Comm
mkComm time = Comm time 0 False


launchWorker :: Job
             -> TVar Comm
             -> [Interval]
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
              -> [Interval]
              -> ProcessHandle
              -> IO ()
monitorWorker job comm intervals workerHandle = do
   threadDelay 100000  -- 100 ms delay
   exitCode <- getProcessExitCode workerHandle
   case exitCode of
      Nothing -> do
         intervals'  <- mapM (checkInterval job) intervals
         comm' <- liftIO $ atomically $ readTVar comm
         if commTerminate comm'
            then do
               putStrLn "Terminating..."
               terminateProcess workerHandle
               reportResults comm
            else monitorWorker job comm intervals' workerHandle
      Just ExitSuccess     -> do
         mapM_ (\x -> (intervalWorker x) job) intervals
      Just (ExitFailure c) -> do
         performRecovery job intervals
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
               launchWorker job comm (map (\x -> x { intervalLastTime = curTime }) intervals)


checkInterval :: Job
              -> Interval
              -> IO (Interval)
checkInterval job interval = do
   curTime <- getCurrentTime
   let diffTime = diffUTCTime curTime (intervalLastTime interval)
   interval' <- if diffTime > (intervalMinTime interval)
      then do
         (intervalWorker interval) job
         return $ interval { intervalLastTime = curTime }
      else return interval
   return $ interval'


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
                -> [Interval]
                -> IO ()
performRecovery job intervals = do
   putStrLn "Recovering data..."
   mapM_ (\x -> (intervalWorker x) job) intervals
