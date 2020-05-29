{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Yaml                      ( ParseException
                                                , decodeFileEither
                                                )
import           Network.Wai.Handler.Warp       ( run )

import           System.IO                      ( hSetBuffering
                                                , stdout
                                                , BufferMode(NoBuffering)
                                                )

import           Config                         ( Config(templates) )
import           RequestHandler                 ( handler )
import           Templates                      ( compileTemplates )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  parsedConfig <-
    decodeFileEither "config.yaml" :: IO (Either ParseException Config)
  case parsedConfig of
    Left  exception -> print exception
    Right config    -> do
      templates <- compileTemplates ["./templates"] $ templates config
      let port = 7777
      putStrLn $ "Listening on port " ++ show port
      run port $ handler templates
