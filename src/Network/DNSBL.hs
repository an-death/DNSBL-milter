{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.DNSBL
    ( withRBLProviders
    , withRBLProvider
    , withDefaultResolver
    ) where

import           Control.Concurrent.Async (mapConcurrently)
import           Data.Either              (either)
import           Data.IP
import           Data.List                (foldr)

import qualified Data.ByteString.Builder  as B
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as BL

import           Network.DNS

type DomainStr = String
type ProviderName = DomainStr

withDefaultResolver :: (Resolver -> IO a) -> IO a
withDefaultResolver f = makeResolvSeed resolverConf >>= \rc -> withResolver rc f
  where
    resolverConf =
        defaultResolvConf { resolvCache = Just defaultCacheConf }


withRBLProviders :: [ProviderName] -> DomainStr -> IO [Either DNSError String]
withRBLProviders providers domain =
    withDefaultResolver $ \resolver -> do
        resolved <- lookupA resolver $ BS.pack domain
        mapConcurrently
            (\p -> check' resolved $ \ip -> withRBLProvider' resolver p ip)
            providers

withRBLProvider :: ProviderName -> DomainStr -> IO (Either DNSError String)
withRBLProvider provider domain =
    withDefaultResolver $ \resolver -> do
        resolved <- lookupA resolver $ BS.pack domain
        check' resolved $ \ip -> withRBLProvider' resolver provider ip


-- | TODO: support IPv6
withRBLProvider' :: Resolver -> DomainStr -> IPv4 -> IO (Either DNSError String)
withRBLProvider' resolver provider ip4 = do
    let checkDomain = joinProviderAndAddr provider ip4
    r <- lookupA resolver checkDomain
    check' r $ \_ -> do
        txt <- lookupTXT resolver checkDomain
        return $
            (\x -> "bad-" <> provider <> "_" <> x) <$>
            fmap (BS.unpack . BS.concat) txt

check' ::
       Either DNSError [a]
    -> (a -> IO (Either DNSError String))
    -> IO (Either DNSError String)
check' current nxt = check current (nxt' nxt)

nxt' _ []       = return $ Right "clean-"
nxt' nxt [a]    = nxt a
nxt' nxt (a:xs) = nxt a

check :: (Monad m) => Either e a -> (a -> m (Either e b)) -> m (Either e b)
check = flip (either (return . Left))

-- | Joining provider domain address with resolved domain address with reversed octets
--   11.22.33.44 + domain.com =>  44.33.22.11.domain.com
--
-- >>> joinProviderAndAddr "domain.com" (read "11.22.33.44" :: IPv4)
-- "44.33.22.11.domain.com"
joinProviderAndAddr :: DomainStr -> IPv4 -> BS.ByteString
joinProviderAndAddr provider addr =
    BL.toStrict . B.toLazyByteString $
    foldr (+.+) providerBS . reverse $ ipToListInts addr
  where
    providerBS = B.stringUtf8 provider
    p = B.char8 '.'
    infixr 6 +.+
    l +.+ r = l <> p <> r
    ipToListInts = map B.intDec . fromIPv4
