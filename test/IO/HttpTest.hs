{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.HttpTest where

-- import ClassyPrelude
-- 
import Test.Tasty

-- import Test.Tasty.HUnit
-- import Brok.IO.Http    (check)
-- import Brok.Types.Link (Link (Link), LinkType (Working), urlToLink)
--
--
test_http :: TestTree
test_http = testGroup "Brok.IO.Http" []
{-
    testGroup
        "Brok.IO.Http"
        [ testCase "Medium (409 with HEAD)" $ do
              result <-
                  check $
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
                  check $
                  urlToLink
                      "https://code.tutsplus.com/tutorials/stateful-vs-stateless-functional-components-in-react--cms-29541"
              assertEqual
                  "Returns a 200"
                  (Link
                       "https://code.tutsplus.com/tutorials/stateful-vs-stateless-functional-components-in-react--cms-29541"
                       (Working 200))
                  result
        ]
-}
