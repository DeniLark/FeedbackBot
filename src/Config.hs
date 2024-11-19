{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
import Data.Yaml (ParseException, decodeFileEither)
import GHC.Generics (Generic)

newtype Config = Config
  { configPort :: Int
  }
  deriving (Show, Generic)

fieldModifier :: String -> String
fieldModifier "configPort" = "port"
fieldModifier s = s

instance FromJSON Config where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }

getConfig :: IO (Either ParseException Config)
getConfig = decodeFileEither "config.yaml"