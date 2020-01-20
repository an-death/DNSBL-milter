module Main where

import System.Environment (getArgs)

import Data.ByteString.Char8 (unpack)
import           Control.Exception       (bracket)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Network.Milter          as Milter
    ( MessageModificator
    , MilterHandler(..)
    , Response(..)
    , defaultMilterHandler
    , milter
    )
import qualified Network.Milter.Protocol as Opt (Action(..), Protocol(..))

import qualified Network.Simple.TCP      as TCP (HostPreference(Host), serve)
import           Network.Socket          (socketToHandle)
import           System.IO
    (BufferMode(NoBuffering), IOMode(ReadWriteMode), hClose, hSetBuffering)

import Network.DNSBL 

main :: IO ()
main = do
  [_, host, _, port] <- getArgs
  withDefaultResolver $ \resolver -> 
          TCP.serve (TCP.Host host ) port $ \(connectionSocket, remoteAddr) ->
            bracket
              (openHandle connectionSocket)
              closeHandle
              (\hdl -> do
                 hSetBuffering hdl NoBuffering
                 putStrLn $ "TCP connection established from " ++ show remoteAddr
                 Milter.milter (myMilter resolver) hdl
                 putStrLn $ "milter Done " ++ show remoteAddr)

openHandle = flip socketToHandle ReadWriteMode

closeHandle = hClose

myMilter resolver = Milter.defaultMilterHandler {Milter.open = open, Milter.eom = eom, Milter.connection=connect resolver}

open :: (MonadIO m) => m Milter.Response
open =
  liftIO $ do
    putStrLn "Milter opened from "
    let onlyConnect = foldr1 (<>) [
          Opt.NoHelo
                                  , Opt.NoMailFrom ,
          Opt.NoRcptTo , Opt.NoBody , Opt.NoHeaders , Opt.NoEOH
                                  ]
    return $ Milter.Negotiate  2 Opt.NoAction  onlyConnect

eom :: (MonadIO m) => Milter.MessageModificator -> m Milter.Response
eom _ =
  liftIO $ do
    putStrLn "DATA BODY END"
    putStrLn "accepted"
    return Milter.Accept

-- connect :: (MonadIO m) => Resolver -> Milter.Content -> Milter.MessageModificator -> m Milter.ResponsePacket
connect resolver ip _ = liftIO $ do
        c <- withRBLProviders ["zen.spamhaus.org"] (unpack ip)
        print c
        return Milter.Accept
