{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (configRoot)
import Control.Concurrent (forkIO, isEmptyMVar, newEmptyMVar, newMVar, takeMVar, withMVar)
import Control.Monad.Reader (ReaderT (runReaderT), when)
import Service (initialize, runService)
import System.Exit (exitFailure)
import System.FSNotify (watchTree, withManager)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  templatesCache <- newEmptyMVar
  parseException <- newEmptyMVar
  let serviceState = (templatesCache, parseException)
  forkIO $ runReaderT runService serviceState
  lock <- newMVar ()
  withManager $ \mgr -> do
    watchTree
      mgr
      configRoot
      (const True)
      ( \_ -> do
          isNotLocked <- not <$> isEmptyMVar lock
          when isNotLocked $ withMVar lock (\_ -> runReaderT initialize serviceState)
      )
    exception <- takeMVar parseException
    print exception
    exitFailure
