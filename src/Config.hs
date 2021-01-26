{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON)
import Data.Yaml (ParseException, decodeFileEither)
import GHC.Generics (Generic)

newtype Config = Config {templates :: [String]} deriving (Generic)

instance FromJSON Config

configFile :: FilePath
configFile = "config.yaml"

type ConfigParseResult = Either ParseException Config

parseConfig :: IO ConfigParseResult
parseConfig = decodeFileEither configFile
