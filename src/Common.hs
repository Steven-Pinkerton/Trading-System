module Common (
  Article (..),
) where


data Article = Article
  { title :: Text
  , url :: Text
  , content :: Text
  }
  deriving stock (Eq, Show)