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

import Data.Text as T (Text, pack, replace, toLower)
import System.Environment (getArgs)

import Control.Concurrent.Async (concurrently)
import Control.Monad (void)

import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

import HTTP (app)
import Milter (newMilter)
import RBL (Provider(..), ProviderResponse, lookupDomain, name, withProviders)

appname :: String
appname = "DNSBL-milter"

main :: IO ()
main = do
  host:port:_ <- getArgs
  print $ "Start " ++ appname ++ " on " ++ host ++ ":" ++ port
  metric <- registerMetrics
  withProviders [Provider ("spamhaus", "zen.spamhaus.org")] $ \rbl ->
    let check = lookupDomain rbl
        output = instrumentMetric metric check
        http = run 6000 (P.prometheus P.def (app appname output))
        milter = newMilter host port output
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
total = P.counter $ P.Info (withname "total_check") "The number of checked IP's."

blacklisted :: P.Metric P.Counter
blacklisted =
  P.counter $
  P.Info (withname "blacklisted") "The count of blacklisted IP's from total."

blacklist :: P.Metric (P.Vector P.Label1 P.Counter)
blacklist =
  P.vector "provider" $
  P.counter $
  P.Info (withname "blacklist") "The count of matched IP's by provider."

withname :: String -> T.Text
withname descr = T.toLower . T.replace "-" "_" . T.pack $ appname ++ "_" ++ descr

instrumentMetric :: Metrics -> (String -> IO [ProviderResponse])
                 -> String -> IO [ProviderResponse]
instrumentMetric m f domain = do
  incTotal m
  res <- f domain
  case res of
    [] -> return ()
    ps -> mapM_ (incBlacklist m . name) ps
  return res
