{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude

import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit

import Brok (links)

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
                         ["http://google.com"]
                         (links "http://google.com"))
              , testCase
                    "just https link"
                    (assertEqual
                         "Gives back google.com"
                         ["https://google.com"]
                         (links "https://google.com"))
              , testCase
                    "http link with surrounding text"
                    (assertEqual
                         "Gives back google.com"
                         ["http://google.com"]
                         (links "A link to Google http://google.com - doesn't it look nice"))
              , testCase
                    "https link with surrounding text"
                    (assertEqual
                         "Gives back google.com"
                         ["https://google.com"]
                         (links "A link to Google https://google.com - doesn't it look nice"))
              , expectFail $
              -- will fail as URLs can contain brackets
                testCase
                    "markdown"
                    (assertEqual
                         "Gives back google.com"
                         ["http://google.com"]
                         (links "[A link](http://google.com)"))
              ]
        , testGroup
              "multiple links"
              [ testCase
                    "links with surrounding text"
                    (assertEqual
                         "Gives back google.com"
                         ["https://google.com", "http://spoons.com"]
                         (links
                              "A link to Google https://google.com - doesn't it look like http://spoons.com"))
              ]
        ]
