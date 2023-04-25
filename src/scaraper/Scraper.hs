{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scaraper.Scraper
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

-- | 'fetchPage' takes a URL as input and fetches the content of the web page. It returns the content as a 'ByteString'.
fetchPage :: Text -> IO ByteString
fetchPage url = do
    req <- parseRequest (toString url)
    res <- httpBS req
    return $ getResponseBody res


-- | 'extractLinks' takes the HTML content of a web page as a ByteString and
-- extracts all the links found within <a> tags.
-- It returns a list of URLs as Text values.    
extractLinks :: ByteString -> [Text]
extractLinks html = [
    T.decodeUtf8 link | TagOpen "a"
    attrs <- parseTags html,
    (_, link) <- attrs, "href" `Data.Text.isPrefixOf` T.decodeUtf8 link
    ]
