module CLI where

import Data.ByteString.Char8 as B (pack)
import Data.List (elemIndex)
import Data.Text as T (pack)

import Options.Applicative
import RBL (Domain, Provider(..))

data AppOpts = AppOpts
  { optMilter :: !HostPort
  , optHttp :: !HostPort
  , optProviders :: ![Provider Domain]
  }

data HostPort = HostPort
  { optHost :: !String
  , optPort :: !Int
  }

parseCLIParams :: IO AppOpts
parseCLIParams = execParser opts
  where
    opts = info (mkParams <**> helper) (fullDesc <> progDesc "DNS RBL milter")

mkParams :: Parser AppOpts
mkParams =
  AppOpts <$>
  option
    parseHostPort
    (long "milter-host-port" <> short 'm' <> help "milter tcp param" <>
     metavar "{host}:{port}") <*>
  option
    parseHostPort
    (long "http-host-port" <>
     help
       "HTTP host-pot for internal API. /metrics for example. Default: localhost:6000" <>
     metavar "{host}:{port}" <>
     value (HostPort {optHost = "localhost", optPort = 6000})) <*>
  some
    (argument
       parseProviderStr
       (metavar "Providers..." <>
        help "List of colon separated providers {name}:{domain}"))
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
