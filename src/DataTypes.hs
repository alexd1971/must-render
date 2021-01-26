module DataTypes where

import Data.Yaml (ParseException)
import Control.Concurrent ( MVar )
import Text.Mustache ( Template )

type ServiceState = (MVar [Template], MVar ParseException)
