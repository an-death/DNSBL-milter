{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module HTTP
  ( app
  ) where

import Control.Exception (displayException,catch)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy as LBS (ByteString)
import Data.Map as Map (fromList)

import qualified Data.Aeson as DA
import Data.IP
import Network.HTTP.Types (status200, status404, status500, Status)
import Network.Wai

import RBL (ProviderResponse, pname, pvalue, Provider(..),ResolveError)

type CheckDomain = String -> IO [ProviderResponse]

data JsonResponse =
  JsonResponse
        {err ::String
        , result :: [IP]} deriving (Show)

instance DA.ToJSON JsonResponse where
        toJSON (JsonResponse e r) = 
                DA.object ["error" DA..= e 
                          , "result" DA..= (show r)
                          ] 

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
check f request = checkDomain domain >>= 
        \(status, content) -> return $ responseLBS status headers content
  where 
    badRequest :: (Status, LBS.ByteString)
    badRequest = (status404, jsonError "Invalid query")
    headers = [("Content-Type", "application/json")]
    query = queryString request
    domain = lookup "domain" query >>= fmap unpack
    checkDomain Nothing = return badRequest
    checkDomain (Just domain') = (f domain' >>= return . response. fmap mapResult) `catch` handleErr 
    mapResult = 
            let mapErr =  displayException 
                mapOk  = id
             in (either mapErr mapOk)
    response results = let json = DA.encode $ Map.fromList $ (,) <$> pname <*> pvalue <$> results
                        in (status200, json)
    handleErr :: ResolveError -> IO (Status, LBS.ByteString)
    handleErr e = return (status500, jsonError (displayException e))
jsonError :: String -> LBS.ByteString
jsonError e = DA.encode $ DA.object ["error" DA..= e]
