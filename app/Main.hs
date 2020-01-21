{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import Data.ByteString (ByteString, hGetLine)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (hPut)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T (pack, replace, toLower)

import Data.Either (rights)

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Network.Simple.TCP as TCP (HostPreference(Host), serve)
import Network.Socket (socketToHandle)
import System.IO
  ( BufferMode(NoBuffering)
  , Handle
  , IOMode(ReadWriteMode)
  , hClose
  , hSetBuffering
  )

import Milter (newMilter)
import Network.DNSBL

import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

import Network.HTTP.Types (status200, status404)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)

name :: String
name = "DNSBL-milter"

main :: IO ()
main = do
  host:port:rblProviders <- getArgs
  print $ "Start " ++ name ++ " on " ++ host ++ ":" ++ port
  (inCn, outCn) <- U.newChan
  metrics <- registerMetrics
  forkIO $ dnsBLResolve metrics outCn rblProviders
  forkIO $ run 6000 (P.prometheus P.def app)
  let handler = newMilter (U.writeChan inCn)
--  let handler = \hdl -> hGetLine hdl >>= U.writeChan inCn
  server host port handler

app :: Wai.Application
app request respond = do
  response <- case Wai.pathInfo request of
      [] -> return $ Wai.responseLBS status200 [("Content-Type", "text/plain")] (pack name)
      _  -> return $ Wai.responseLBS status404 [("Content-Type", "text/plain")] "sorry"
  respond response

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

dnsBLResolve :: Metrics -> U.OutChan ByteString -> [String] -> IO ()
dnsBLResolve metric chan providers = loop
  where
    loop = do
      ip <- U.readChan chan
      incTotal metric ()
      c <- withRBLProviders providers (unpack ip)
      case rights c of
        [] -> return ()
        provider -> do
          incBlacklisted metric ()
          mapM_ (incBlacklist metric) $ map T.pack provider
      loop

data Metrics =
  Metrics
    { incTotal :: () -> IO ()
    , incBlacklisted :: () -> IO ()
    , incBlacklist :: P.Label1 -> IO ()
    }

registerMetrics = do
  _ <- P.register P.ghcMetrics
  _total <- P.register total
  _blacklisted <- P.register blacklisted
  _blacklist <- P.register blacklist
  let inc = const . P.incCounter
  return $
    Metrics (inc _total) (inc _blacklisted) (\p -> P.withLabel _blacklist p P.incCounter)

total :: P.Metric P.Counter
total =
  P.counter $ P.Info (withname "total_check") "The number of checked IP's."

blacklisted :: P.Metric P.Counter
blacklisted =
  P.counter $
  P.Info (withname "blacklisted") "The count of blacklisted IP's from total."

blacklist :: P.Metric (P.Vector P.Label1 P.Counter)
blacklist =
  P.vector (withname "blacklist") $
  P.counter $
  P.Info (withname "blacklist") "The count of matched IP's by provider."

withname descr = T.toLower . T.replace "-" "_" . T.pack $ name ++ "_" ++ descr
