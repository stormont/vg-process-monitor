
module System.Process.Monitor
   where

import qualified Data.ByteString.Lazy as B
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


doWork = do
   putStrLn "Hello"
   launchWorker
   putStrLn "Done"


launchWorker = do
   (_, _, _, h) <- createProcess (proc "temp.exe" []) { std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }
   exitCode <- waitForProcess h
   case exitCode of
      ExitSuccess   -> return ()
      ExitFailure c -> do
         performRecovery
         launchWorker


performRecovery = do
   putStrLn "Does some recovery tasks..."
