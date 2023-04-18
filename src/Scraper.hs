{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scraper
    ( fetchPage
    , extractLinks
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text, isPrefixOf) -- Import isPrefixOf from Data.Text
import Data.Text.Encoding as T (decodeUtf8)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Text.HTML.TagSoup (Tag (TagOpen), parseTags)
import Relude
    ( ($),
      Monad(return),
      IO,
      ToString(toString) )

fetchPage :: Text -> IO ByteString
fetchPage url = do
    req <- parseRequest (toString url)
    res <- httpBS req
    return $ getResponseBody res

extractLinks :: ByteString -> [Text]
extractLinks html = [
    T.decodeUtf8 link | TagOpen "a"
    attrs <- parseTags html,
    (_, link) <- attrs, "href" `Data.Text.isPrefixOf` T.decodeUtf8 link
    ]
