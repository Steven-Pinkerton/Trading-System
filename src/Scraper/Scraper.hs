{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use alternative" #-}

module Scraper.Scraper (
  extractLinks,
) where

import Data.ByteString (ByteString)
import Data.Text (Text, isPrefixOf) -- Import isPrefixOf from Data.Text
import Data.Text.Encoding as T (decodeUtf8)
import Text.HTML.TagSoup (Tag (TagOpen), parseTags)


{- | 'extractLinks' takes the HTML content of a web page as a ByteString and
 extracts all the links found within <a> tags.
 It returns a list of URLs as Text values.
-}
extractLinks :: ByteString -> [Text]
extractLinks html =
  [ decodeUtf8 link
  | TagOpen
      "a"
      attrs <-
      parseTags html
  , (_, link) <- attrs
  , "href" `Data.Text.isPrefixOf` decodeUtf8 link
  ]
