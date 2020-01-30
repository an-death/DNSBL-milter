{-# LANGUAGE FlexibleInstances #-}

module HTTP
  ( app
  ) where

import Control.Exception (catch, displayException)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy as LBS (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Map as Map (fromList)

import qualified Data.Aeson as DA
import Data.IP
import Network.HTTP.Types (Status, status200, status404, status500)
import Network.Wai

import RBL (Provider(..), ProviderResponse, ResolveError, pname, pvalue)

type CheckDomain = String -> IO [ProviderResponse]

data JsonResponse = JsonResponse
  { err :: String
  , result :: [IP]
  } deriving (Show)

instance DA.ToJSON JsonResponse where
  toJSON (JsonResponse e r) = DA.object ["error" DA..= e, "result" DA..= show r]

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
index appname =
  responseLBS status200 [("Content-Type", "text/plain")] (pack appname)

notFound :: Response
notFound = responseLBS status404 [("Content-Type", "text/plain")] "¯\\_(ツ)_/¯"

check :: CheckDomain -> Request -> IO Response
check f request =
  checkDomain domain >>= \(status, content) ->
    return $ responseLBS status headers content
  where
    badRequest :: (Status, LBS.ByteString)
    badRequest = (status404, jsonError "Invalid query")
    headers = [("Content-Type", "application/json")]
    query = queryString request
    domain = lookup "domain" query >>= fmap unpack
    checkDomain Nothing = return badRequest
    checkDomain (Just domain') =
      (response . mapResult <$> f domain') `catch` handleErr
    mapResult =
      let mapErr = fmap (pure . displayException)
          mapOk = fmap (map show)
       in fmap (either mapErr mapOk)
    response results =
      let json = DA.encode dict
          dict = Map.fromList $ (,) <$> pname <*> pvalue <$> results
       in (status200, json)
    handleErr :: ResolveError -> IO (Status, LBS.ByteString)
    handleErr e = return (status500, jsonError (displayException e))

jsonError :: String -> LBS.ByteString
jsonError e = DA.encode $ DA.object ["error" DA..= e]
