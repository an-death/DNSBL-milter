{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.DNSBL
  ( lookupProvider
  , withDefaultResolver
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IP
import Data.List (foldr)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import qualified Network.DNS as DNS


type DomainStr = String

withDefaultResolver :: (DNS.Resolver -> IO a) -> IO a
withDefaultResolver f =
  DNS.makeResolvSeed resolverConf >>= \rc -> DNS.withResolver rc f
  where
    resolverConf =
      DNS.defaultResolvConf {DNS.resolvCache = Just DNS.defaultCacheConf}

-- | TODO: support IPv6
lookupProvider :: (MonadIO m) => DNS.Resolver -> DomainStr -> IPv4 -> m (Maybe String)
lookupProvider resolver provider ip4 = do
  let checkDomain = joinProviderAndAddr provider ip4
  r <- liftIO $ DNS.lookupA resolver checkDomain
  case r of
    Left DNS.NameError -> return Nothing
    Right _ -> return $ Just provider
    Left e -> liftIO $ throwIO e

-- | Joining provider domain address with resolved domain address with reversed octets
--   11.22.33.44 + domain.com =>  44.33.22.11.domain.com
--
-- >>> joinProviderAndAddr "domain.com" (read "11.22.33.44" :: IPv4)
-- "44.33.22.11.domain.com"
joinProviderAndAddr :: DomainStr -> IPv4 -> BS.ByteString
joinProviderAndAddr provider ip = toByteString $ foldr (+.+) providerBS reversedIP
  where
    reversedIP = reverse $ ipToListInts ip
    providerBS = B.stringUtf8 provider
    toByteString = BL.toStrict . B.toLazyByteString 
    p = B.char8 '.'
    infixr 6 +.+
    l +.+ r = l <> p <> r
    ipToListInts = map B.intDec . fromIPv4
