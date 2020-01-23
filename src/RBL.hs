{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module RBL (
        RBL
           , Provider(..)
           , Providers
           , Name, Domain, ProviderResponse
           , lookupA, lookupDomain
           , withProviders
        ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Data.IP
import qualified Network.DNS as DNS

newtype Provider a = Provider ByteString deriving (Show, Eq)
data Name
data Domain
type ProviderResponse = [IP]

type Providers = [(Provider Name, Provider Domain)]
        
data RBL = RBL
        { _providers :: Providers
        , _resolver :: DNS.Resolver
        }

withDefaultResolver :: (DNS.Resolver -> IO a) -> IO a
withDefaultResolver f =
  DNS.makeResolvSeed resolverConf >>= \rc -> DNS.withResolver rc f
  where
    resolverConf =
      DNS.defaultResolvConf {DNS.resolvCache = Just DNS.defaultCacheConf}

withProviders :: Providers -> (RBL -> IO a) -> IO a
withProviders providers f = 
        withDefaultResolver $ 
                \resolver -> f $ RBL {_providers = providers, _resolver = resolver}

lookupIP :: RBL -> IPv4 -> IO [(Provider Name, ProviderResponse)]
lookupIP (RBL ps resolver) ip = 
        filter (not . null.snd) <$> mapConcurrently lookupProvider ps
    where 
        lookupProvider :: (Provider Name, Provider Domain) -> IO (Provider Name, ProviderResponse)
        lookupProvider (name, domain) = do
            let checkDomain = joinProviderAndAddr domain ip
            resp <- lookupA resolver checkDomain
            return $ case resp of 
              Nothing -> (name, [])
              Just ips -> (name, IPv4 <$> ips)

lookupDomain :: RBL -> String -> IO [(Provider Name, ProviderResponse)]
lookupDomain rbl@(RBL _ resolver) domain = do
    ip <- lookupA resolver domainBS
    when (isNothing ip) $ error (domain ++ " not resolved")
    results <- mapConcurrently (lookupIP rbl) (fromJust ip)
    return $ concat results
    where
        domainBS = BS.pack domain

lookupA :: DNS.Resolver -> ByteString -> IO (Maybe [IPv4])
lookupA resolver domain = do
    resp <- DNS.lookupA resolver domain
    case resp of 
      -- name not found
      Left DNS.NameError -> return Nothing
      Right ansv         -> return $ Just ansv
      Left e             -> throwIO e

-- | Joining provider domain address with resolved domain address with reversed octets
--   11.22.33.44 + domain.com =>  44.33.22.11.domain.com
--
-- >>> joinProviderAndAddr "domain.com" (read "11.22.33.44" :: IPv4)
-- "44.33.22.11.domain.com"
joinProviderAndAddr :: Provider Domain -> IPv4 -> ByteString
joinProviderAndAddr (Provider domain) ip = 
        toByteString $ foldr (+.+) providerBS reversedIP
  where
    reversedIP = reverse $ ipToListInts ip
    providerBS = B.byteString domain
    toByteString = BL.toStrict . B.toLazyByteString 
    p = B.char8 '.'
    infixr 6 +.+
    l +.+ r = l <> p <> r
    ipToListInts = map B.intDec . fromIPv4
