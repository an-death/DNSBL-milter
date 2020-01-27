{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Main where

import System.Environment (getArgs)

import Data.ByteString.Char8 as B (pack)
import Data.List (elemIndex)
import Data.Text as T (Text, pack, replace, toLower)

import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (throwString)
import Control.Monad (void)

import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

import HTTP (app)
import Milter (newMilter)
import RBL (Domain, Provider(..), ProviderResponse, lookupDomain, withProviders)

appname :: String
appname = "DNSBL-milter"

parseProviders :: [String] -> IO [Provider Domain]
parseProviders = mapM parseProvider

parseProvider :: String -> IO (Provider Domain)
parseProvider provider =
  case elemIndex ':' provider of
    Nothing ->
      throwString $
      "cannot parse provider. Expected {name}:{domain}, got: " ++ provider
    (Just idx) ->
      let (name, ':':domain) = splitAt idx provider
       in return $ Provider {pname = T.pack name, pvalue = B.pack domain}

main :: IO ()
main = do
  host:port:xs <- getArgs
  putStrLn $ "Start " ++ appname ++ " on " ++ host ++ ":" ++ port
  metric <- registerMetrics
  providers <- parseProviders xs
  withProviders providers $ \rbl ->
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
total =
  P.counter $ P.Info (withname "total_check") "The number of checked IP's."

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
withname descr = cleanMetricName . T.pack $ appname ++ "_" ++ descr

cleanMetricName :: T.Text -> T.Text
cleanMetricName = T.toLower . T.replace "-" "_"

instrumentMetric ::
     Metrics -> (a -> IO [ProviderResponse]) -> a -> IO [ProviderResponse]
instrumentMetric m f domain = do
  incTotal m
  res <- f domain
  case res of
    [] -> return ()
    ps -> mapM_ (incBlacklist m . pname) ps
  return res
