{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module HTTP
  ( app
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T

import qualified Data.Aeson as DA

import Network.HTTP.Types (status200, status404, status400)
import Network.Wai

import Data.IP
import RBL (Provider(..), Name)

type CheckDomain = ByteString -> IO [(Provider Name, [IP])]

newtype JsonProvider = JsonProvider (Provider Name)
newtype JsonIP = JsonIP IP

instance DA.ToJSON JsonProvider where
        toJSON (JsonProvider (Provider bs)) = DA.String $ decodeUtf8 bs 

instance DA.ToJSON JsonIP where 
        toJSON (JsonIP ip) = DA.String . T.pack $ show ip


app :: String -> CheckDomain -> Application
app name checkF request respond = do
  let index' = index name
  response <-
    case pathInfo request of
      [] -> return index'
      ["check"] -> check checkF request
      _ -> return $ notFound
  respond response

index :: String -> Response
index name = responseLBS status200 [("Content-Type", "text/plain")] (pack name)

notFound :: Response
notFound = responseLBS status404 [("Content-Type", "text/plain")] "¯\\_(ツ)_/¯"

check :: CheckDomain -> Request -> IO Response
check f request = do
  let query = queryString request
  case lookup "domain" query of
    Nothing -> badRequest
    Just (Nothing) -> badRequest
    Just (Just domain) -> do
      res <- f domain
      response res
  where
    badRequest =
      return $
      responseLBS status400 [("Content-Type", "text/plain")] "Invalid query"
    response ips = return $ responseLBS status200 headers json 
        where 
            json = DA.encode $ map (\(a, b)-> (JsonProvider a, map JsonIP b))ips
            headers = [("Content-Type", "application/json")]
