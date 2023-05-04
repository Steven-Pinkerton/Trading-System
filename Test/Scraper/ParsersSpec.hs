{-# LANGUAGE OverloadedStrings #-}

module Scraper.ParsersSpec where

import Test.Hspec
import Scraper.Parsers ( extractArticlesGamesIndustry )
import Common ( Article(..))

spec :: Spec
spec = do
  describe "extractArticlesGamesIndustry" $ do
    it "extracts articles from sample GamesIndustry HTML" $ do
      let sampleHtml = "\
  \<!DOCTYPE html>\n\
  \<html lang=\"en\">\n\
  \<head>\n\
    \<meta charset=\"UTF-8\">\n\
    \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n\
    \<title>Sample HTML</title>\n\
  \</head>\n\
  \<body>\n\
    \<article>\n\
      \<h1>Title 1</h1>\n\
      \<a href=\"https://www.example.com/article1\">Read more</a>\n\
      \<p>Content of the first article.</p>\n\
    \</article>\n\
    \<article>\n\
      \<h1>Title 2</h1>\n\
      \<a href=\"https://www.example.com/article2\">Read more</a>\n\
      \<p>Content of the second article.</p>\n\
    \</article>\n\
    \<div class=\"summary\" data-type=\"article\" data-article-type=\"news\" data-premium=\"false\" data-external=\"false\" data-size=\"default\">\n\
      \<a href=\"https://www.gamesindustry.biz/super-mario-bros-movie-grosses-1bn-globally\" class=\"link_overlay\" title=\"Super Mario Bros. Movie grosses $1bn globally\"></a>\n\
      \<div class=\"thumbnail\">\n\
        \<img src=\"https://assetsio.reedpopcdn.com/SMB-film-2023.png?width=400&amp;height=225&amp;fit=crop&amp;quality=70&amp;format=jpg&amp;auto=webp\" srcset=\"https://assetsio.reedpopcdn.com/SMB-film-2023.png?width=400&amp;height=225&amp;fit=crop&amp;quality=70&amp;format=jpg&amp;auto=webp 1x, https://assetsio.reedpopcdn.com/SMB-film-2023.png?width=400&amp;height=225&amp;fit=crop&amp;quality=70&amp;format=jpg&amp;dpr=2&amp;auto=webp 2x\" loading=\"lazy\" class=\"thumbnail_image\" alt=\"Image for Super Mario Bros. Movie grosses $1bn globally\" width=\"400\" height=\"225\">\n\
      \</div>\n\
      \<div class=\"wrapper\">\n\
        \<div class=\"details\">\n\
          \<p class=\"title\">\n\
            \<a href=\"https://www.gamesindustry.biz/super-mario-bros-movie-grosses-1bn-globally\">Super Mario Bros. Movie grosses $1bn globally</a>\n\
          \</p>\n\
          \<p class=\"strapline\">The Nintendo and Illumination film hits the revenue milestone after nearly a month in theaters</p>\n\
          \<p class=\"author\">Jeffrey Rousseau</p>\n\
        \</div>\n\
        \<div class=\"metadata\">\n\
          \<p class=\"article_type\" data-slug=\"news\">News</p>\n\
          \<p class=\"published_at\">\n\
            \<time datetime=\"2023-05-01T19:49:03+00:00\">16 hours ago</time>\n\
          \</p>\n\
        \</div>\n\
      \</div>\n\
    \</div>\n\
    \<div class=\"summary\" data-type=\"article\" data-article-type=\"news\" data-premium=\"false\" data-external=\"false\" data-size=\"default\">\n\
      \<a href=\"https://www.gamesindustry.biz/meta-sees-engagement-issues-with-quest-headsets\" class=\"link_overlay\" title=\"Meta sees engagement issues with Quest headsets\"></a>\n\
      \<div class=\"thumbnail\">\n\
        \<img src=\"https://assetsio.reedpopcdn.com/Meta-quest-pro.png?width=400&amp;height=225&amp;fit=crop&amp;quality=70&amp;format=jpg&amp;auto=webp\" srcset=\"https://assetsio.reedpopcdn.com/Meta-quest-pro.png?width=400&amp;height=225&amp;fit=crop&amp;quality=70&amp;format=jpg&amp;auto=webp 1x, https://assetsio.reedpopcdn.com/Meta-quest-pro.png?width=400&amp;height=225&amp;fit=crop&amp;quality=70&amp;format=jpg&amp;dpr=2&amp;auto=webp 2x\" loading=\"lazy\" class=\"thumbnail_image\" alt=\"Image for Meta sees engagement issues with Quest headsets\" width=\"400\" height=\"225\">\n\
      \</div>\n\
      \<div class=\"wrapper\">\n\
        \<div class=\"details\">\n\
          \<p class=\"title\">\n\
            \<a href=\"https://www.gamesindustry.biz/meta-sees-engagement-issues-with-quest-headsets\">Meta sees engagement issues with Quest headsets</a>\n\
          \</p>\n\
          \<p class=\"strapline\">New reports say that users of the VR devices would stop using them after a few weeks</p>\n\
          \<p class=\"author\">Jeffrey Rousseau</p>\n\
        \</div>\n\
        \<div class=\"metadata\">\n\
          \<p class=\"article_type\" data-slug=\"news\">News</p>\n\
          \<p class=\"published_at\">\n\
            \<time datetime=\"2023-05-01T19:00:44+00:00\">17 hours ago</time>\n\
          \</p>\n\
        \</div>\n\
      \</div>\n\
    \</div>\n\
  \</body>\n\
\</html>"
      let expectedArticles = 
        [ Article 
            { title = "Super Mario Bros. Movie grosses $1bn globally"
            , url = "https://www.gamesindustry.biz/super-mario-bros-movie-grosses-1bn-globally"
            , content = "The Nintendo and Illumination film hits the revenue milestone after nearly a month in theaters"
        }
        , Article
            { title = "Meta sees engagement issues with Quest headsets"
            , url = "https://www.gamesindustry.biz/meta-sees-engagement-issues-with-quest-headsets"
            , content = "New reports say that users of the VR devices would stop using them after a few weeks"
            }
        ]
      extractArticlesGamesIndustry sampleHtml `shouldBe` expectedArticles