{-# LANGUAGE OverloadedStrings #-}

module RequestHandler where

import           Control.Monad.Reader           ( Reader
                                                , ask
                                                )
import           Data.Aeson                     ( Value(Null)
                                                , decode
                                                )
import           Data.ByteString.Lazy.UTF8      ( fromString )
import           Data.ByteString.UTF8           ( toString )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( unpack )
import           Network.HTTP.Types             ( status200
                                                , status400
                                                , status404
                                                )
import           Network.HTTP.Types.Header      ( hContentType )
import           Network.HTTP.Types.Status      ( Status )
import           Network.Wai                    ( Request
                                                , Response
                                                , Application
                                                , queryString
                                                , responseLBS
                                                , strictRequestBody
                                                , requestMethod
                                                )
import           Text.Mustache                  ( Template
                                                , substitute
                                                )
import           Text.Mustache.Types            ( mFromJSON )

import           Templates

-- | Достает имя шаблона из параметров запроса
getTemplateName :: Request -> Maybe String
getTemplateName req =
  let params = filter (\x -> fst x == "t") (queryString req)
  in  if null params then Nothing else toString <$> snd (head params)

-- | Формирует ответ сервера
buildResponse :: Status -> String -> Response
buildResponse status text =
  responseLBS status [(hContentType, "text/plain")] $ fromString text

-- | Обработчик запросов к сервису
handler :: Reader [Template] Application
handler = do
  templates <- ask
  return $ \req respond -> if requestMethod req /= "POST"
    then respond $ buildResponse status400 "Use POST-request"
    else
      let maybeTemplate = do
            name <- getTemplateName req
            findTemplate templates name
      in  case maybeTemplate of
            Nothing -> respond $ buildResponse
              status404
              "Template not found: check if it exists and present in query"
            Just template -> do
              body <- strictRequestBody req
              let values = fromMaybe Null $ decode body
              respond $ buildResponse status200 $ unpack $ substitute
                template
                (mFromJSON values)
