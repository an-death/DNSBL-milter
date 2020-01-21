module Main where

import System.Environment (getArgs)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Network.Milter as Milter
  ( MessageModificator
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
  , IOMode(ReadWriteMode)
  , Handle
  , hClose
  , hSetBuffering
  )

import Network.DNSBL

name = "DNSBL-milter"

main :: IO ()
main = do
  [_, host, _, port] <- getArgs
  print $ "Start " ++ name ++ " on " ++ host ++ ":" ++ port
  (inCn, outCn) <- U.newChan
  forkIO $ dnsBLResolve outCn
  let handler = Milter.milter (myMilter inCn)
  server host port handler

server :: String -> String -> (Handle -> IO ()) -> IO ()
server host port handler =
  TCP.serve (TCP.Host host) port $ \(connectionSocket, remoteAddr) ->
    bracket
      (openHandle connectionSocket remoteAddr)
      (closeHandle remoteAddr)
      handler
  where
    openHandle soc remoteAddr = do
      hdl <- socketToHandle soc ReadWriteMode
      hSetBuffering hdl NoBuffering
      putStrLn $ "TCP connection established from " ++ show remoteAddr
      return hdl
    closeHandle remoteAddr hdl = do
      hClose hdl
      putStrLn $ "connection done" ++ show remoteAddr

dnsBLResolve :: U.OutChan ByteString -> IO ()
dnsBLResolve cn = loop
  where
    loop = do
      ip <- U.readChan cn
      c <- withRBLProviders ["zen.spamhaus.org"] (unpack ip)
      print c
      loop

myMilter cn =
  Milter.defaultMilterHandler
    {Milter.open = open, Milter.eom = eom, Milter.connection = connect cn}

open :: (MonadIO m) => m Milter.Response
open =
  liftIO $ do
    putStrLn "Milter opened from "
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
    return $ Milter.Negotiate 2 Opt.NoAction onlyConnect

eom :: (MonadIO m) => Milter.MessageModificator -> m Milter.Response
eom _ =
  liftIO $ do
    putStrLn "DATA BODY END"
    putStrLn "accepted"
    return Milter.Accept

connect ::
     (MonadIO m)
  => U.InChan ByteString
  -> ByteString
  -> Milter.MessageModificator
  -> m Milter.Response
connect cn ip _ =
  liftIO $ do
    U.writeChan cn ip
    return Milter.Accept
