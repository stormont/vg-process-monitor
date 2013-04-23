
module Main
   where

import qualified Data.ByteString.Lazy.Char8 as B

import System.Process.Monitor


main = do
   let worker = Executable (B.pack "temp.exe") []
       job = Job worker Nothing Nothing Nothing Nothing
   doWork job
