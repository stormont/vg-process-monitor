{-# LANGUAGE OverloadedStrings #-}
module Main
   where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans (liftIO)
import Data.Time
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment (getArgs)

import System.Process.Monitor


main = do
   args <- getArgs
   let port = (read $ head args) :: Int
   let cmd = drop 1 args
   let intervals = []
   putStrLn $ "Monitoring: " ++ show cmd
   comm <- startMonitor cmd intervals
   putStrLn $ "Monitor server running on port " ++ show port
   run port (app comm)


app
   :: TVar Comm
   -> Application
app comm req resp = do
   case pathInfo req of
      ("stop":_) -> do
         liftIO $ terminateWorker comm
         liftIO $ putStrLn "Monitoring completed."
         resp stopR
      _ -> resp defaultR


stopR
   :: Response
stopR =
   responseLBS
      status200
      [(hContentType, "text/plain")]
      "Stopped"


defaultR
   :: Response
defaultR =
   responseLBS
      status200
      [(hContentType, "text/plain")]
      ""


startMonitor
   :: [String]
   -> [Interval]
   -> IO (TVar Comm)
startMonitor args intervals = do
   putStrLn "Monitoring beginning..."
   time <- getCurrentTime
   comm <- liftIO $ atomically $ newTVar $ mkComm time
   forkIO $ doWork args comm intervals
   return comm


doWork
   :: [String]
   -> TVar Comm
   -> [Interval]
   -> IO ()
doWork (x:xs) comm intervals = do
   let worker = Executable x xs
       job = mkJob worker
   launchWorker job comm intervals
