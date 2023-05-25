{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use alternative" #-}
{-# HLINT ignore "Use 'lenientDecode' from Relude" #-}
{-# HLINT ignore "Use 'decodeUtf8With' from Relude" #-}

module Scraper.Scraper (
  extractLinks,
) where

import Data.Text.Encoding as T (decodeUtf8With)
import Network.URI (URI (..), parseURI)
import Text.HTML.TagSoup (Tag (TagOpen), parseTags)

{- | 'extractLinks' takes the HTML content of a web page as a ByteString and
 extracts all the links found within <a> tags.
 It returns a list of URLs as Text values.

 Input:
 * ByteString: HTML content of a web page

 Output:
 * [Text]: List of URLs extracted from the HTML content

 Side Effects: 
 * Can throw an exception if the ByteString is not valid UTF-8.

 Note: This function does not check whether the extracted URLs are valid URLs.
-}
extractLinks :: ByteString -> [URI]
extractLinks html =
  mapMaybe (parseURI . toString . T.decodeUtf8With lenientDecode) links
  where
    links = [link | TagOpen "a" attrs <- parseTags html, ("href", link) <- attrs]