{-# LANGUAGE OverloadedStrings #-}

module ArticleExtraction.Preprocessing (
  preprocess,
) where

import Data.Char (isAlphaNum, isPunctuation)
import Data.Text qualified as T
import NLP.Snowball (stem, Algorithm (English))
import NLP.Tokenize.Text (tokenize)
import Text.HTML.TagSoup (innerText, parseTags)

-- | 'preprocess' function takes a 'Text' and returns a list of preprocessed tokens as 'Text'.
preprocess :: Text -> [Text]
preprocess text =
  let noHtml = removeHtmlTags text
      noPunctuation = removePunctuation noHtml
      lower = toLowerCase noPunctuation
      tokens = tokenize lower
      noSpecialChars = map removeSpecialChars tokens
      stemmed = map stemWord noSpecialChars
      noStopwords = removeStopwords stemmed
   in noStopwords

-- | 'removeHtmlTags' function takes a 'Text' and returns 'Text' without HTML tags.
removeHtmlTags :: Text -> Text
removeHtmlTags text = innerText $ parseTags text

-- | 'removePunctuation' function takes a 'Text' and returns 'Text' without punctuation.
removePunctuation :: Text -> Text
removePunctuation = T.filter (not . isPunctuation)

-- | 'toLowerCase' function takes a 'Text' and returns 'Text' with all characters in lowercase.
toLowerCase :: Text -> Text
toLowerCase = T.toLower

-- | 'removeSpecialChars' function takes a 'Text' and returns 'Text' without special characters.
removeSpecialChars :: Text -> Text
removeSpecialChars = T.filter isAlphaNum

-- | 'stemWord' function takes a 'Text' and returns the stemmed 'Text'.
stemWord :: Text -> Text
stemWord word = stem English (word :: Text) :: Text

-- | 'removeStopwords' function takes a list of tokens and returns the list without stop words.
removeStopwords :: [Text] -> [Text]
removeStopwords = filter (`notElem` stopwords)
  where
    stopwords = ["a", "an", "and", "the", "in", "is", "it", "this", "that", "of", "for", "on"] -- Add more stop words as needed