{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Strict #-}

module RBL
  ( RBL
  , Provider(..)
  , Providers
  , Name
  , Domain
  , ProviderResponse
  , lookupA
  , lookupDomain
  , withProviders
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (throwIO)
import Control.Exception.Safe (catch, throwString)
import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Data.IP
import qualified Network.DNS as DNS

data Provider a =
  Provider
    { pname :: !Name
    , pvalue :: !a
    }
  deriving (Functor)

type Name = T.Text

type Domain = ByteString

type Response = [IP]

type Providers = [Provider Domain]

type ProviderResponse = Provider Response

data RBL =
  RBL
    { _providers :: !Providers
    , _resolver :: !DNS.Resolver
    }

withDefaultResolver :: (DNS.Resolver -> IO a) -> IO a
withDefaultResolver f =
  DNS.makeResolvSeed resolverConf >>= \rc -> DNS.withResolver rc f
  where
    resolverConf =
      DNS.defaultResolvConf {DNS.resolvCache = Just DNS.defaultCacheConf}

withProviders :: Providers -> (RBL -> IO a) -> IO a
withProviders providers f =
  withDefaultResolver $ \resolver ->
    f $ RBL {_providers = providers, _resolver = resolver}

lookupIP :: RBL -> IPv4 -> IO [ProviderResponse]
lookupIP (RBL ps resolver) ip =
  dropWhile (null . pvalue) <$> mapConcurrently lookupProvider ps
  where
    lookupProvider :: Provider Domain -> IO ProviderResponse
    lookupProvider provider = do
      let checkDomain = joinProviderAndAddr (pvalue provider) ip
      resp <- lookupA resolver checkDomain
      let ips = maybe [] (map IPv4) resp
      return $ ips <$ provider

lookupDomain :: RBL -> String -> IO [ProviderResponse]
lookupDomain rbl@(RBL _ resolver) domain = do
  ip <- lookupA resolver domainBS
  when (isNothing ip) $ throwString (domain ++ " not resolved")
  results <- mapConcurrently lookupIPSafe (fromJust ip)
  return $ concat results
  where
    domainBS = BS.pack domain
    lookupIPSafe ip =
      lookupIP rbl ip `catch` (\(_ :: DNS.DNSError) -> return [])

lookupA :: DNS.Resolver -> ByteString -> IO (Maybe [IPv4])
lookupA resolver domain = do
  resp <- DNS.lookupA resolver domain
  case resp of
    Left DNS.NameError -> return Nothing  -- name not found
    Right ansv -> return $ Just ansv
    Left e -> throwIO e

-- | Joining provider domain address with resolved domain address with reversed octets
--   11.22.33.44 + domain.com =>  44.33.22.11.domain.com
--
-- >>> joinProviderAndAddr "domain.com" (read "11.22.33.44" :: IPv4)
-- "44.33.22.11.domain.com"
joinProviderAndAddr :: Domain -> IPv4 -> ByteString
joinProviderAndAddr domain ip = toByteString $ foldr (+.+) providerBS reversedIP
  where
    reversedIP = reverse $ ipToListInts ip
    providerBS = B.byteString domain
    toByteString = BL.toStrict . B.toLazyByteString
    p = B.char8 '.'
    infixr 6 +.+
    l +.+ r = l <> p <> r
    ipToListInts = map B.intDec . fromIPv4
