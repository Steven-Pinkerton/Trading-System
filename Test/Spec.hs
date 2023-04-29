module Spec where

import qualified ArticleExtractionSpec
import Test.Hspec ( hspec, describe )
import WebScraperSpec ( spec )

main :: IO ()
main = hspec $ do
  describe "WebScraper" WebScraperSpec.spec
  describe "ArticleExtraction" ArticleExtractionSpec.spec