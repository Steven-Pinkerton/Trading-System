{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use alternative" #-}
{-# HLINT ignore "Use 'lenientDecode' from Relude" #-}
{-# HLINT ignore "Use 'decodeUtf8With' from Relude" #-}

module Scraper.Scraper (
  extractLinks,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding as T (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Text.HTML.TagSoup (Tag (TagOpen), parseTags)

{- | 'extractLinks' takes the HTML content of a web page as a ByteString and
 extracts all the links found within <a> tags.
 It returns a list of URLs as Text values.
-}
extractLinks :: ByteString -> [Text]
extractLinks html =
  [  decodeUtf8With lenientDecode link
  | TagOpen "a" attrs <- parseTags html
  , ("href", link) <- attrs
  ]