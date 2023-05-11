{-# LANGUAGE DeriveGeneric #-}

module Common (
  Article (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text ()

data Article = Article
  { title :: Text
  , url :: Text
  , content :: Text
  }
  deriving stock (Eq, Show, Generic)

-- We derive these instances so that we can convert Article to/from JSON
instance ToJSON Article
instance FromJSON Article