module Main where

import System.Environment (getArgs)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Network.Simple.TCP as TCP (HostPreference(Host), serve)
import Network.Socket (socketToHandle)
import System.IO
  ( BufferMode(NoBuffering)
  , IOMode(ReadWriteMode)
  , Handle
  , hClose
  , hSetBuffering
  )

import Network.DNSBL

import Milter (newMilter)

name = "DNSBL-milter"

main :: IO ()
main = do
  [_, host, _, port] <- getArgs
  print $ "Start " ++ name ++ " on " ++ host ++ ":" ++ port
  (inCn, outCn) <- U.newChan
  forkIO $ dnsBLResolve outCn ["zen.spamhaus.org"] 
  let handler = newMilter (U.writeChan inCn)
  server host port handler

server :: String -> String -> (Handle -> IO ()) -> IO ()
server host port handler =
  TCP.serve (TCP.Host host) port $ \(connectionSocket, remoteAddr) ->
    bracket
      (openHandle connectionSocket remoteAddr)
      (closeHandle remoteAddr)
      handler
  where
    openHandle soc remoteAddr = do
      hdl <- socketToHandle soc ReadWriteMode
      hSetBuffering hdl NoBuffering
      putStrLn $ "TCP connection established from " ++ show remoteAddr
      return hdl
    closeHandle remoteAddr hdl = do
      hClose hdl
      putStrLn $ "connection done" ++ show remoteAddr

dnsBLResolve :: U.OutChan ByteString -> [String] -> IO ()
dnsBLResolve cn providers= loop
  where
    loop = do
      ip <- U.readChan cn
      c <- withRBLProviders providers (unpack ip) 
      print c
      loop

