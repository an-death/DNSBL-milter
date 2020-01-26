{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module RBL
  ( RBL
  , Provider(..)
  , Providers
  , Name
  , Domain
  , ProviderResponse
  , ResolveError
  , lookupA
  , lookupDomain
  , withProviders
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Safe (throwM, Typeable,)
import Control.Exception (Exception(..),ArrayException(IndexOutOfBounds))
import Control.Monad.IO.Class (MonadIO, liftIO)
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
  deriving (Functor, Show)

type Name = T.Text

type Domain = ByteString

type Response = [IP]

type Providers = [Provider Domain]

type ProviderResponse = Either (Provider ResolveError) (Provider Response)

data ResolveError = forall e d. (Exception e, Show d) => ResolveError d e
    deriving Typeable
instance  Show ResolveError where
    show (ResolveError dom e) = "Domain " ++ show dom ++ " not resolved. Reason: " ++ displayException e
instance  Exception ResolveError


data RBL =
  RBL
    { _providers :: !Providers
    , _resolve :: forall m. (MonadIO m) => ByteString -> m (Either DNS.DNSError [IPv4])
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
    f $ RBL {_providers = providers, _resolve = lookupA resolver}

lookupA :: (MonadIO m) => DNS.Resolver -> ByteString -> m (Either DNS.DNSError [IPv4])
lookupA resolver = liftIO . (fmap parse). DNS.lookupA resolver
   where
     parse (Left DNS.NameError) = Right []
     parse a = a

lookupDomain :: RBL -> String -> IO [ProviderResponse]
lookupDomain rbl@(RBL _ resolve) domain = do
  res <- resolve domainBS
  case res of
    Left e -> throwResolveError e
    Right [] -> throwEmptyIPError 
    Right ips -> concat <$> mapConcurrently (lookupIP rbl) ips
  where
    domainBS = BS.pack domain
    throwResolveError e = throwM $ ResolveError domain e
    throwEmptyIPError = throwM $ ResolveError domain (IndexOutOfBounds "no IP resolved") 

lookupIP :: RBL -> IPv4 -> IO [ProviderResponse]
lookupIP (RBL ps resolve) ip =
  mapConcurrently lookupProvider ps
  where
    lookupProvider :: Provider Domain -> IO ProviderResponse
    lookupProvider provider = do
      let checkDomain = joinProviderAndAddr (pvalue provider) ip
      resolve checkDomain >>= \resp -> return $
              case resp of 
                Left e   -> Left $ ResolveError checkDomain e <$ provider
                Right xs -> Right$ map IPv4 xs <$ provider

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
