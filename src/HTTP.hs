{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module HTTP
  ( app
  ) where

import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Map as Map (fromList)
import qualified Data.Text as T

import qualified Data.Aeson as DA

import Network.HTTP.Types (status200, status400, status404)
import Network.Wai

import Data.IP
import RBL (ProviderResponse, pname, pvalue)

type CheckDomain = String -> IO [ProviderResponse]

newtype JsonIP =
  JsonIP IP
  deriving (Show)

instance DA.ToJSON JsonIP where
  toJSON (JsonIP ip) = DA.String . T.pack $ show ip

app :: String -> CheckDomain -> Application
app appname checkF request respond = do
  let index' = index appname
  response <-
    case pathInfo request of
      [] -> return index'
      ["check"] -> check checkF request
      _ -> return notFound
  respond response

index :: String -> Response
index appname = responseLBS status200 [("Content-Type", "text/plain")] (pack appname)

notFound :: Response
notFound = responseLBS status404 [("Content-Type", "text/plain")] "¯\\_(ツ)_/¯"

check :: CheckDomain -> Request -> IO Response
check f request =
  case lookup "domain" query of
    Just (Just domain) -> f (unpack domain) >>= response
    _                  -> badRequest
  where
    query = queryString request
    response checkresults =
      let json =
            DA.encode $
            Map.fromList $ (,) <$> pname <*> (JsonIP <$>) . pvalue <$> checkresults
          headers = [("Content-Type", "application/json")]
       in return $ responseLBS status200 headers json
    badRequest =
      return $
      responseLBS status400 [("Content-Type", "text/plain")] "Invalid query"
