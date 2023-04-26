{-# LANGUAGE OverloadedStrings #-}

module Preprocessing (
  preprocess,
) where

import Data.Text qualified as T
import NLP.Tokenize.Text (tokenize)
import Text.HTML.TagSoup (innerText, parseTags)
import Data.Char (isAlphaNum)

-- | 'preprocess' function takes a 'Text' and returns a list of preprocessed tokens as 'Text'.
preprocess :: Text -> [Text]
preprocess text =
  let noHtml = removeHtmlTags text
      lower = toLowerCase noHtml
      tokens = tokenize lower
      noSpecialChars = map removeSpecialChars tokens
      noStopwords = removeStopwords noSpecialChars
   in noStopwords

-- | 'removeHtmlTags' function takes a 'Text' and returns 'Text' without HTML tags.
removeHtmlTags :: Text -> Text
removeHtmlTags text = innerText $ parseTags text

-- | 'toLowerCase' function takes a 'Text' and returns 'Text' with all characters in lowercase.
toLowerCase :: Text -> Text
toLowerCase = T.toLower

-- | 'removeSpecialChars' function takes a 'Text' and returns 'Text' without special characters.
removeSpecialChars :: Text -> Text
removeSpecialChars = T.filter isAlphaNum

-- | 'removeStopwords' function takes a list of tokens and returns the list without stop words.
removeStopwords :: [Text] -> [Text]
removeStopwords = filter (`notElem` stopwords)
  where
    stopwords = ["a", "an", "and", "the", "in", "is", "it", "this", "that", "of", "for", "on"] -- Add more stop words as needed
