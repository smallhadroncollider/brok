{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.HttpTest where

import ClassyPrelude

import Test.Tasty

import Brok.IO.Http     (check)
import Brok.Types.Link  (Link (Link), LinkType (..), urlToLink)
import Test.Tasty.HUnit

test_http :: TestTree
test_http =
    testGroup
        "Brok.IO.Http"
        [ testCase "Medium (409 with HEAD)" $ do
              result <-
                  check 0 $
                  urlToLink
                      "https://medium.freecodecamp.org/understanding-redux-the-worlds-easiest-guide-to-beginning-redux-c695f45546f6"
              assertEqual
                  "Returns a 200"
                  (Link
                       "https://medium.freecodecamp.org/understanding-redux-the-worlds-easiest-guide-to-beginning-redux-c695f45546f6"
                       (Working 200))
                  result
        , testCase "TutsPlus (Requires User-Agent Header)" $ do
              result <-
                  check 0 $
                  urlToLink
                      "https://code.tutsplus.com/tutorials/stateful-vs-stateless-functional-components-in-react--cms-29541"
              assertEqual
                  "Returns a 200"
                  (Link
                       "https://code.tutsplus.com/tutorials/stateful-vs-stateless-functional-components-in-react--cms-29541"
                       (Working 200))
                  result
        , testCase "Random blog (404 on a HEAD request)" $ do
              result <-
                  check 0 $
                  urlToLink
                      "https://blog.infinitenegativeutility.com/2017/12/some-notes-about-how-i-write-haskell"
              assertEqual
                  "Returns a 200"
                  (Link
                       "https://blog.infinitenegativeutility.com/2017/12/some-notes-about-how-i-write-haskell"
                       (Working 200))
                  result
        , testCase "Non-existant site" $ do
              result <- check 0 $ urlToLink "http://askdjfhaksjdhfkajsdfh.com"
              assertEqual
                  "Returns a 200"
                  (Link "http://askdjfhaksjdhfkajsdfh.com" ConnectionFailure)
                  result
        , testCase "Invalid URL" $ do
              result <-
                  check 0 $
                  urlToLink "http://user:password&#64;securesite.com/secret-file.json&quot;"
              assertEqual
                  "Returns a 200"
                  (Link "http://user:password&#64;securesite.com/secret-file.json&quot;" InvalidURL)
                  result
        ]
