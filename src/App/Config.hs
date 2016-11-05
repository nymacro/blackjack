{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Config where

import           GHC.Generics

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map         (Map)
import           Data.Text        (Text)

data LoginConfig = LoginConfig { name    :: Text
                               , options :: Maybe (Map Text Text) }
  deriving (Generic, Show)

instance FromJSON LoginConfig
