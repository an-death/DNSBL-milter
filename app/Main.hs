#!/usr/bin/env stack
{- stack
  --resolver lts-14.20
  --install-ghc
  runghc
  --
  localhost
  8000
  zen.spamhaus.org
  +RTS -T
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Main where

import System.Environment (getArgs)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T (Text, pack, replace, toLower)

import Control.Concurrent.Async (Concurrently(..), runConcurrently)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Exception (bracket, catch)
import Control.Monad (void)

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
import qualified Network.DNS as DNS
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
  let resolver = dnsBLResolve metrics (U.readChan outCn) rblProviders
      httpserver = run 6000 (P.prometheus P.def app)
      handler = newMilter (U.writeChan inCn)
      --handler = \hdl -> hGetLine hdl >>= U.writeChan inCn
      milterserver = server host port handler
  void $ concurrent3 resolver httpserver milterserver

app :: Wai.Application
app request respond = do
  response <- case Wai.pathInfo request of
      []        -> return $ Wai.responseLBS status200 [("Content-Type", "text/plain")] (pack name)
      ["check"] -> return $ Wai.responseLBS status200 [("Content-Type", "text/plain")] (pack name)
      _         -> return $ Wai.responseLBS status404 [("Content-Type", "text/plain")] "sorry"
  respond response

server :: String -> String -> (Handle -> IO ()) -> IO ()
server host port handler =
  TCP.serve (TCP.Host host) port $ \(connectionSocket, remoteAddr) ->
    bracket
      (openHandle connectionSocket)
      closeHandle
      (\hdl -> do
         putStrLn $ "TCP connection established from " ++ show remoteAddr
         handler hdl
         putStrLn $ "connection done" ++ show remoteAddr)
  where
    openHandle soc = do
      hdl <- socketToHandle soc ReadWriteMode
      hSetBuffering hdl NoBuffering
      return hdl
    closeHandle = hClose

dnsBLResolve :: Metrics -> IO ByteString -> [String] -> IO ()
dnsBLResolve metric chan providers =
  withDefaultResolver $ \resolver -> loop resolver
  where
    loop resolver = do
      ip <- chan
      incTotal metric
      c <- lookupProvider resolver (head providers) (read $ unpack ip) `catch` \(e :: DNS.DNSError) ->
          print e >> return Nothing
      case c of
        Nothing -> return ()
        Just provider -> do
          incBlacklisted metric
          incBlacklist metric $ T.pack provider
      loop resolver

data Metrics =
  Metrics
    { incTotal :: IO ()
    , incBlacklisted :: IO ()
    , incBlacklist :: P.Label1 -> IO ()
    }

registerMetrics :: IO Metrics
registerMetrics = do
  _ <- P.register P.ghcMetrics
  _total <- P.register total
  _blacklisted <- P.register blacklisted
  _blacklist <- P.register blacklist
  let inc = P.incCounter
  return $ Metrics (inc _total) (inc _blacklisted) (\p -> P.withLabel _blacklist p P.incCounter)

total :: P.Metric P.Counter
total = P.counter $ P.Info (withname "total_check") "The number of checked IP's."

blacklisted :: P.Metric P.Counter
blacklisted =
  P.counter $
  P.Info (withname "blacklisted") "The count of blacklisted IP's from total."

blacklist :: P.Metric (P.Vector P.Label1 P.Counter)
blacklist =
  P.vector (withname "blacklist") $
  P.counter $
  P.Info (withname "blacklist") "The count of matched IP's by provider."

withname :: String -> T.Text
withname descr = T.toLower . T.replace "-" "_" . T.pack $ name ++ "_" ++ descr

concurrent3 :: IO a -> IO b -> IO c -> IO (a, b, c)
concurrent3 a1 a2 a3 = runConcurrently $ (,,)
    <$> Concurrently a1
    <*> Concurrently a2
    <*> Concurrently a3
