{-# LANGUAGE DeriveGeneric #-}

module Common (
  Article (..),
convertTag) where

import Data.Aeson (FromJSON, ToJSON)
import Text.HTML.TagSoup ( Tag(..) )

data Article = Article
  { title :: Text
  , url :: Text
  , content :: Text
  }
  deriving stock (Eq, Show, Generic)

-- We derive these instances so that we can convert Article to/from JSON
instance ToJSON Article
instance FromJSON Article

convertTag :: Tag String -> Tag Text
convertTag (TagOpen str attrs) = TagOpen (toText str) (map (bimap toText toText) attrs)
convertTag (TagClose str) = TagClose (toText str)
convertTag (TagText str) = TagText (toText str)
convertTag (TagComment str) = TagComment (toText str)
convertTag (TagWarning str) = TagWarning (toText str)
convertTag (TagPosition row col) = TagPosition row col
