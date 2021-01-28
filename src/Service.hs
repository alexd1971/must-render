module Service where

import Config (parseConfig)
import Control.Concurrent (isEmptyMVar, putMVar)
import Control.Monad.Reader (ReaderT (runReaderT), asks, lift, when)
import DataTypes (ServiceState)
import Handler (mkHandler)
import Network.Wai.Handler.Warp (run)
import Templates (cacheTemplates)

-- | Запускает сервис
runService :: ReaderT ServiceState IO ()
runService = do
  initialize
  exception <- asks snd
  initSuccess <- lift $ isEmptyMVar exception
  when initSuccess $ do
    let port = 7777
    lift . putStrLn $ "Listening on port " ++ show port
    handler <- mkHandler
    lift $ run port handler

-- | Инициализирует состояние сервиса
initialize :: ReaderT ServiceState IO ()
initialize = do
  lift . putStr $ "Reading config..."
  parseResult <- lift parseConfig
  case parseResult of
    Left parseException -> do
      lift . putStrLn $ "failed"
      exception <- asks snd
      lift . putMVar exception $ parseException
    Right config -> do
      lift . putStrLn $ "done"
      lift . putStr $ "Reading templates..."
      templatesCache <- asks fst
      lift $ runReaderT (cacheTemplates templatesCache) config
      lift . putStrLn $ "done"
