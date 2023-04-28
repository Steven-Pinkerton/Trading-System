module Main where

import qualified ArticleExtractionSpec
import Test.Hspec
import qualified WebScraperSpec

main :: IO ()
main = hspec $ do
  describe "WebScraper" WebScraperSpec.spec
  describe "ArticleExtraction" ArticleExtractionSpec.spec