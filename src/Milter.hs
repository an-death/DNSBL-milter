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
import Network.Milter.Protocol (getIP, getBody)

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

newMilter :: (Foldable t) => String -> String -> (String -> IO (t a)) -> IO ()
newMilter host port send =
  server host port $
  MilterLib.milter
    MilterLib.defaultMilterHandler
      { MilterLib.open = open
      , MilterLib.eom = eom
      , MilterLib.connection = connect send
      , MilterLib.helo = helo send
      }

open :: (MonadIO m) => m MilterLib.Response
open =
  liftIO $ do
    let onlyConnect =
          foldr1
            (<>)
            [
             Opt.NoMailFrom
            , Opt.NoRcptTo
            , Opt.NoBody
            , Opt.NoHeaders
            , Opt.NoEOH
            ]
    return $ MilterLib.Negotiate 2 Opt.NoAction onlyConnect

eom :: (MonadIO m) => MilterLib.MessageModificator -> m MilterLib.Response
eom _ =
  liftIO $ do
    return MilterLib.Accept

connect :: (String -> IO a) -> MilterLib.HandleF
connect send ip _ = liftIO $ send ( show $ getIP ip) >> return MilterLib.Continue

helo :: (Foldable t ) => (String -> IO (t a)) -> MilterLib.HandleF
helo send helostr _ = liftIO $ do
  r <- send . unpack . getBody  $! helostr
  case null r of
    True -> return MilterLib.Accept
    _ -> return MilterLib.Reject