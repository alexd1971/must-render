{-# LANGUAGE OverloadedStrings #-}

module Templates
  ( Templates,
    cacheTemplates,
    hasName,
    templatesDir,
  )
where

import Config (Config (templates))
import Control.Concurrent.MVar (MVar, putMVar, tryTakeMVar)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, asks)
import Data.Maybe (catMaybes)
import Text.Mustache (Template, automaticCompile, name)

templatesDir :: FilePath
templatesDir = "./templates"

type Templates = MVar [Template]

{-
  Компилирует шаблоны
  Поиск шаблонов с именами из `templateNames` происходит по каталогам
-}
cacheTemplates :: Templates -> ReaderT Config IO ()
cacheTemplates cacheMVar = do
  templateNames <- asks templates
  templates <- catMaybes <$> lift (mapM compile templateNames)
  lift $ tryTakeMVar cacheMVar
  lift $ putMVar cacheMVar templates

-- | Компилирует один шаблон
compile :: FilePath -> IO (Maybe Template)
compile templateName = do
  compiled <- automaticCompile [templatesDir] templateName
  case compiled of
    Left err -> do
      print err
      return Nothing
    Right template -> return (Just template)

-- | Проверяет соответствует ли имя шаблона переданной строке
hasName :: String -> Template -> Bool
hasName templateName template = name template == templateName
