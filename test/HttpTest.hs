{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Http (LinkStatus (Working), check)

test_http :: TestTree
test_http =
    testGroup
        "http"
        [ testCase "Medium (409 with HEAD)" $ do
              result <-
                  check
                      [ "https://medium.freecodecamp.org/understanding-redux-the-worlds-easiest-guide-to-beginning-redux-c695f45546f6"
                      ]
              assertEqual
                  "Returns a 200"
                  [ ( "https://medium.freecodecamp.org/understanding-redux-the-worlds-easiest-guide-to-beginning-redux-c695f45546f6" :: Text
                    , Working)
                  ]
                  result
        , testCase "TutsPlus (Requires User-Agent Header)" $ do
              result <-
                  check
                      [ "https://code.tutsplus.com/tutorials/stateful-vs-stateless-functional-components-in-react--cms-29541"
                      ]
              assertEqual
                  "Returns a 200"
                  [ ( "https://code.tutsplus.com/tutorials/stateful-vs-stateless-functional-components-in-react--cms-29541" :: Text
                    , Working)
                  ]
                  result
        ]
