module Common (
  Article (..),
) where


data Article = MkArticle
  { title :: Text
  , url :: Text
  , content :: Text
  }
  deriving stock (Eq, Show)