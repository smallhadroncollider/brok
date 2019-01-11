{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import ClassyPrelude

import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit

import Data.FileEmbed (embedFile)

import LinkParser (links)

markdown :: Text
markdown = decodeUtf8 $(embedFile "test/data/links.md")

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "links"
        [ testGroup
              "single links"
              [ testCase
                    "just http link"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com"])
                         (links "(test: Spec.hs)" "http://google.com"))
              , testCase
                    "just https link"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["https://google.com"])
                         (links "(test: Spec.hs)" "https://google.com"))
              , testCase
                    "http link with surrounding text"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com"])
                         (links
                              "(test: Spec.hs)"
                              "A link to Google http://google.com - doesn't it look nice"))
              , testCase
                    "https link with surrounding text"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["https://google.com"])
                         (links
                              "(test: Spec.hs)"
                              "A link to Google https://google.com - doesn't it look nice"))
              , testCase
                    "markdown"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com"])
                         (links "(test: Spec.hs)" "[A link](http://google.com)"))
              , testCase
                    "link with brackets"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q=(fish)"])
                         (links "(test: Spec.hs)" "[A link](http://google.com?q=(fish))"))
              , testCase
                    "link with square brackets surrounding"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q=(fish)"])
                         (links "(test: Spec.hs)" "[http://google.com?q=(fish)]"))
              , testCase
                    "link with square brackets"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q=fish"])
                         (links "(test: Spec.hs)" "[http://google.com?q=fish]"))
              , expectFail $
                -- using surround with the same character on each side causing issues? 
                testCase
                    "link with single quotes"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q='fish'"])
                         (links "(test: Spec.hs)" "http://google.com?q='fish'"))
              , testCase
                    "link with single quotes surrounding"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q=fish"])
                         (links "(test: Spec.hs)" "'http://google.com?q=fish'"))
              ]
        , testGroup
              "multiple links"
              [ testCase
                    "links with surrounding text"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["https://google.com", "http://spoons.com"])
                         (links
                              "(test: Spec.hs)"
                              "A link to Google https://google.com - doesn't it look like http://spoons.com"))
              , expectFail $
              -- not parsing whole file correctly
                testCase
                    "basic markdown"
                    (assertEqual
                         "Gives back google.com"
                         (Right
                              [ "https://google.com"
                              , "http://amazon.com"
                              , "https://www.facebook.com"
                              , "http://www.apple.com"
                              ])
                         (links "(test: links.md)" markdown))
              ]
        ]
