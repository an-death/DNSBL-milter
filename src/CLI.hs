module CLI where

import Data.ByteString.Char8 as B (pack)
import Data.List (elemIndex)
import Data.Text as T (pack)
import GHC.Word (Word32)

import Options.Applicative
import RBL (Domain, Provider(..))

data AppOpts = AppOpts
  { optMilter :: !HostPort
  , optHttpPort :: !Int
  , optResolvConfFile :: !String
  , optCacheDisable :: !Bool
  , optCacheTTL :: !Word32
  , optProviders :: ![Provider Domain]
  }

data HostPort = HostPort
  { optHost :: !String
  , optPort :: !Int
  }

parseCLIParams :: IO AppOpts
parseCLIParams = execParser opts
  where
    opts = info
      (mkParams <**> helper)
      ( fullDesc <>
        header "DNSBL milter for collecting RBL results")

mkParams :: Parser AppOpts
mkParams =
  AppOpts <$>
        option
          parseHostPort
          (  long "milter-host-port"
          <> short 'm'
          <> help "milter TCP server specification"
          <> metavar "{HOST}:{PORT}")
    <*> option
          auto
          (  long "http-port"
          <> help "HTTP port for internal API. Default: localhost:6000"
          <> metavar "PORT"
          <> value 6000)
    <*> option
         str
         (  long "resolv-cfg"
         <> value "/etc/resolv.conf"
         <> metavar "FILEPATH"
         <> help "Specific path to resolv.conf file. Default: /etc/resolve.conf")
    <*> switch
        (  long "disable-cache"
        <> help "Disabling internal cache. Enabled cache may lead a gross of MEM consumption on high load")
    <*> option
         auto
         (  long "cache-ttl"
         <> value 300
         <> metavar "SEC"
         <> help "Time to live internal cache entities in seconds. Default: 300")
    <*> some
        (argument
           parseProviderStr
           (metavar "PROVIDERS..."
           <> help "List of colon separated providers {name}:{domain}"))
  where
    parseHostPort = (HostPort <$> fst <*> read . snd) <$> parseColonSeparated
    parseProviderStr =
      (Provider <$> T.pack . fst <*> B.pack . snd) <$> parseColonSeparated
    parseColonSeparated = eitherReader readColonSeparated

readColonSeparated :: String -> Either String (String, String)
readColonSeparated provider =
  case elemIndex ':' provider of
    Nothing -> Left cannotParse
    Just idx ->
      let (left, ':':right) = splitAt idx provider
       in return (left, right)
  where
    cannotParse = "Cannot parse colon-separated value. " ++ provider
