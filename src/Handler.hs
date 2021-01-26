{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Control.Concurrent (readMVar)
import Control.Monad.Reader ( ReaderT, asks )
import Data.Aeson
  ( Value (Null),
    decode,
  )
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.ByteString.UTF8 (toString)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import DataTypes ( ServiceState )
import Network.HTTP.Types
  ( status200,
    status400,
    status404,
  )
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (Status)
import Network.Wai
  ( Application,
    Request,
    Response,
    queryString,
    requestMethod,
    responseLBS,
    strictRequestBody,
  )
import Templates (hasName)
import Text.Mustache (substitute)
import Text.Mustache.Types (mFromJSON)

-- | Получает имя шаблона из параметров запроса
getTemplateName :: Request -> Maybe String
getTemplateName req =
  let params = filter (\x -> fst x == "t") (queryString req)
   in if null params then Nothing else toString <$> snd (head params)

-- | Формирует ответ сервера
buildResponse :: Status -> String -> Response
buildResponse status text =
  responseLBS status [(hContentType, "text/plain")] $ fromString text

-- | Формирует обработчик запросов к сервису
mkHandler :: ReaderT ServiceState IO Application
mkHandler = do
  templatesIO <- asks (readMVar . fst)
  return
    ( \req respond ->
        if requestMethod req /= "POST"
          then respond $ buildResponse status400 "Use POST-request"
          else do
            templates <- templatesIO
            let maybeTemplate = do
                  name <- getTemplateName req
                  find (hasName name) templates
            case maybeTemplate of
              Nothing ->
                respond $
                  buildResponse
                    status404
                    "Template not found: check if it exists and present in query"
              Just template -> do
                body <- strictRequestBody req
                let values = fromMaybe Null $ decode body
                respond $
                  buildResponse status200 $
                    unpack $
                      substitute
                        template
                        (mFromJSON values)
    )
