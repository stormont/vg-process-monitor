
module System.Process.Monitor
   where

import qualified Data.ByteString.Lazy.Char8 as B
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


type Monitor = [Job]


doWork :: Job -> IO ()
doWork job = do
   putStrLn "Hello"
   launchWorker job
   putStrLn "Done"


launchWorker :: Job -> IO ()
launchWorker job = do
   let worker = jobWorker job
   (_, _, _, h) <- createProcess (proc (B.unpack $ execCmd worker) (map (B.unpack) $ execArgs worker))
                                 { std_out = CreatePipe
                                 , std_in  = CreatePipe
                                 , std_err = CreatePipe
                                 }
   exitCode <- waitForProcess h
   case exitCode of
      ExitSuccess   -> return ()
      ExitFailure c -> do
         performRecovery
         launchWorker job


performRecovery = do
   putStrLn "Does some recovery tasks..."
