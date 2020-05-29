{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Aeson                     ( FromJSON )
import           GHC.Generics                   ( Generic )

newtype Config = Config {templates::[String]} deriving Generic

instance FromJSON Config
