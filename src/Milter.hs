{-# LANGUAGE RankNTypes #-}
module Milter (newMilter) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)

import qualified Network.Milter as MilterLib
  ( MessageModificator
  , MilterHandler(..)
  , HandleFilterF
  , Response(..)
  , defaultMilterHandler
  , milter
  )
import qualified Network.Milter.Protocol as Opt (Action(..), Protocol(..))

import System.IO  (Handle)

newMilter :: (ByteString -> IO()) ->Handle -> IO ()
newMilter send = MilterLib.milter
  MilterLib.defaultMilterHandler
    {MilterLib.open = open, MilterLib.eom = eom, MilterLib.connection = connect send}

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

connect :: (ByteString -> IO()) -> MilterLib.HandleFilterF
connect send ip _ =
  liftIO $ do
    send ip
    return MilterLib.Accept

