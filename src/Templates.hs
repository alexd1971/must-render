{-# LANGUAGE OverloadedStrings #-}

module Templates
  ( compileTemplates
  , findTemplate
  )
where

import           Data.Maybe                     ( catMaybes
                                                , listToMaybe
                                                )
import           Text.Mustache                  ( Template
                                                , automaticCompile
                                                , name
                                                )

-- | Компилирует шаблоны
-- Поиск шаблонов с именами из `templateNames` происходит по каталогам
-- `searchPaths`
compileTemplates :: [FilePath] -> [FilePath] -> IO [Template]
compileTemplates searchPaths templateNames = do
  templates <- mapM (compile searchPaths) templateNames
  return $ catMaybes templates

-- | Компилирует один шаблон
compile :: [FilePath] -> FilePath -> IO (Maybe Template)
compile searchPaths templateName = do
  compiled <- automaticCompile searchPaths templateName
  case compiled of
    Left err -> do
      print err
      return Nothing
    Right template -> return (Just template)

-- | Проверяет соответствует ли имя шаблона переданной строке
hasName :: String -> Template -> Bool
hasName templateName template = name template == templateName

-- | Ищет шаблон по имени
findTemplate :: [Template] -> String -> Maybe Template
findTemplate templates templateName =
  listToMaybe . (filter $ hasName templateName) $ templates
