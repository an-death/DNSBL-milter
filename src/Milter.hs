{-# LANGUAGE RankNTypes #-}

module Milter
  ( newMilter
  ) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString.Char8 (unpack)

import qualified Network.Milter as MilterLib
  ( HandleF
  , MessageModificator
  , MilterHandler(..)
  , Response(..)
  , defaultMilterHandler
  , milter
  )
import qualified Network.Milter.Protocol as Opt (Action(..), Protocol(..))
import Network.Milter.Protocol (getBody, getIP)

import qualified Network.Simple.TCP as TCP (HostPreference(Host), serve)
import Network.Socket (socketToHandle)
import System.IO
  ( BufferMode(NoBuffering)
  , Handle
  , IOMode(ReadWriteMode)
  , hClose
  , hSetBuffering
  )

server :: String -> String -> (Handle -> IO ()) -> IO ()
server host port handler =
  TCP.serve (TCP.Host host) port $ \(connectionSocket, remoteAddr) ->
    bracket
      (openHandle connectionSocket)
      closeHandle
      (\hdl -> do
         putStrLn $ "TCP connection established from " ++ show remoteAddr
         handler hdl
         putStrLn $ "connection done " ++ show remoteAddr)
  where
    openHandle soc = do
      hdl <- socketToHandle soc ReadWriteMode
      hSetBuffering hdl NoBuffering
      return hdl
    closeHandle = hClose

newMilter :: String -> String -> (String -> IO a) -> IO ()
newMilter host port check =
  server host port $
  MilterLib.milter
    MilterLib.defaultMilterHandler
      { MilterLib.open = open
      , MilterLib.eom = eom
      , MilterLib.connection = connect check
      }

open :: (MonadIO m) => m MilterLib.Response
open = return $ MilterLib.Negotiate 2 Opt.NoAction onlyConnect
  where
    onlyConnect =
      foldr1 (<>) [Opt.NoRcptTo, Opt.NoBody, Opt.NoHeaders, Opt.NoEOH]

connect :: (String -> IO a) -> MilterLib.HandleF
connect send ip _ =
  liftIO $
  send (show $ getIP ip) >>
  -- ignore any others checks
  -- after Accept other commands below will be skipped
  return MilterLib.Accept

helo :: (Foldable t) => (String -> IO (t a)) -> MilterLib.HandleF
helo send helostr _ =
  liftIO $ do
    r <- send . unpack . getBody $! helostr
    if null r
      then return MilterLib.Accept
      else return MilterLib.Reject

eom :: (MonadIO m) => MilterLib.MessageModificator -> m MilterLib.Response
eom _ = return MilterLib.Accept
