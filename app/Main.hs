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
import qualified Data.Text as T (Text, pack, replace, toLower)
import Data.Text.Encoding (decodeUtf8)

import Control.Concurrent.Async (concurrently)
import Control.Monad (void)


import Milter (newMilter)
import RBL (lookupDomain, withProviders, Provider(..), Name)

import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

import Data.IP

import HTTP (app)
import Network.Wai.Handler.Warp (run)

name :: String
name = "DNSBL-milter"

main :: IO ()
main = do
  host:port:_ <- getArgs
  print $ "Start " ++ name ++ " on " ++ host ++ ":" ++ port
  metric <- registerMetrics
  withProviders [(Provider "spamhaus",Provider "zen.spamhaus.org")] $ \rbl ->
     let 
        check = lookupDomain rbl . unpack
        http = run 6000 (P.prometheus P.def (app name check))
        output = metrics metric check
        checkWithMetric = output  
        milter = newMilter host port checkWithMetric
     in void $ concurrently milter http

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
  return $
    Metrics
      (inc _total)
      (inc _blacklisted)
      (\p -> P.withLabel _blacklist p P.incCounter)

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

withname :: String -> T.Text
withname descr = T.toLower . T.replace "-" "_" . T.pack $ name ++ "_" ++ descr

metrics :: Metrics -> (ByteString -> IO [(Provider Name, [IP])]) -> ByteString -> IO()
metrics m f domain = do
    incTotal m
    res <- f domain
    case res of 
      [] -> return ()
      ps -> do
          incBlacklisted m
          mapM_ (incBlacklist m) $ map ((\(Provider p) -> decodeUtf8 p) . fst) ps

