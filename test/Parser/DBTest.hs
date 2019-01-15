{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser.DBTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.Either    (isLeft, isRight)
import Data.FileEmbed (embedFile)

import Parser.DB (db)

content :: Text
content = decodeUtf8 $(embedFile "test/data/.brokdb")

invalid :: Text
invalid = decodeUtf8 $(embedFile "test/data/.brokdb-invalid")

big :: Text
big = decodeUtf8 $(embedFile "test/data/.brokdb-big")

test_db :: TestTree
test_db =
    testGroup
        "db"
        [ testCase
              "parses .brokdb"
              (assertEqual
                   "Gives back URLs with timestamp"
                   (Right
                        [ ( "https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle"
                          , 1547542131)
                        , ( "https://www.amazon.co.uk/Code-Language-Computer-Hardware-Software/dp/0735611319"
                          , 1546959165)
                        , ("https://rosettacode.org/wiki/FizzBuzz", 1546440765)
                        , ( "https://medium.com/dailyjs/the-why-behind-the-wat-an-explanation-of-javascripts-weird-type-system-83b92879a8db"
                          , 1546008765)
                        ])
                   (db content))
        , testCase "big file" (assertEqual "Gives back empty array" True (isRight $ db big))
        , testCase "invalid .brokdb" (assertEqual "Parse error" True (isLeft $ db invalid))
        , testCase "parses empty" (assertEqual "Gives back empty array" (Right []) (db ""))
        ]
