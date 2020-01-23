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
         putStrLn $ "connection done" ++ show remoteAddr)
  where
    openHandle soc = do
      hdl <- socketToHandle soc ReadWriteMode
      hSetBuffering hdl NoBuffering
      return hdl
    closeHandle = hClose

newMilter :: String -> String -> (String -> IO a) -> IO ()
newMilter host port send =
  server host port $
  MilterLib.milter
    MilterLib.defaultMilterHandler
      { MilterLib.open = open
      , MilterLib.eom = eom
      , MilterLib.connection = connect send
      }

open :: (MonadIO m) => m MilterLib.Response
open =
  liftIO $ do
    putStrLn "MilterLib.opened from "
    let onlyConnect =
          foldr1
            (<>)
            [ Opt.NoHelo
            , Opt.NoMailFrom
            , Opt.NoRcptTo
            , Opt.NoBody
            , Opt.NoHeaders
            , Opt.NoEOH
            ]
    return $ MilterLib.Negotiate 2 Opt.NoAction onlyConnect

eom :: (MonadIO m) => MilterLib.MessageModificator -> m MilterLib.Response
eom _ =
  liftIO $ do
    putStrLn "DATA BODY END"
    putStrLn "accepted"
    return MilterLib.Accept

connect :: (String -> IO a) -> MilterLib.HandleF
connect send ip _ = liftIO $ send (unpack ip) >> return MilterLib.Accept
