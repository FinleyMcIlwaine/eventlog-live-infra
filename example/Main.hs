{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import System.Environment

import GHC.Eventlog.Socket qualified

main :: IO ()
main = do
  socketPath <-
    lookupEnv "GHC_EVENTLOG_SOCKET" >>= \case
      Nothing -> return "/tmp/ghc-eventlog-socket"
      Just p  -> return p
  GHC.Eventlog.Socket.start socketPath
  _ <- getLine
  return ()
