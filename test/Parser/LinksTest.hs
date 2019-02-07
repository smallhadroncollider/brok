{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser.LinksTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit

import Data.FileEmbed (embedFile)

import Brok.Parser.Links (links)

markdown :: Text
markdown = decodeUtf8 $(embedFile "test/data/links.md")

complex :: Text
complex = decodeUtf8 $(embedFile "test/data/complex.md")

tex :: Text
tex = decodeUtf8 $(embedFile "test/data/links.tex")

test_parser :: TestTree
test_parser =
    testGroup
        "Brok.Parser.Links"
        [ testGroup
              "single links"
              [ testCase
                    "just http link"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com"])
                         (links "http://google.com"))
              , testCase
                    "just https link"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["https://google.com"])
                         (links "https://google.com"))
              , testCase
                    "http link with surrounding text"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com"])
                         (links "A link to Google http://google.com - doesn't it look nice"))
              , testCase
                    "https link with surrounding text"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["https://google.com"])
                         (links "A link to Google https://google.com - doesn't it look nice"))
              , testCase
                    "markdown"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com"])
                         (links "[A link](http://google.com)"))
              , testCase
                    "link with brackets"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q=(fish)"])
                         (links "[A link](http://google.com?q=(fish))"))
              , testCase
                    "link with square brackets surrounding"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q=(fish)"])
                         (links "[http://google.com?q=(fish)]"))
              , testCase
                    "link with square brackets"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q=fish"])
                         (links "[http://google.com?q=fish]"))
              , expectFail $
                -- using surround with the same character on each side causing issues? 
                testCase
                    "link with single quotes"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q='fish'"])
                         (links "http://google.com?q='fish'"))
              , testCase
                    "link with single quotes surrounding"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com?q=fish"])
                         (links "'http://google.com?q=fish'"))
              , expectFail $
                testCase
                    "link with comma on end"
                    (assertEqual
                         "Gives back google.com"
                         (Right ["http://google.com"])
                         (links "testing http://google.com, here"))
              ]
        , testGroup
              "multiple links"
              [ testCase
                    "links with surrounding text"
                    (assertEqual
                         "Gives back two URLs"
                         (Right ["https://google.com", "http://spoons.com"])
                         (links
                              "A link to Google https://google.com - doesn't it look like http://spoons.com"))
              , testCase
                    "basic markdown"
                    (assertEqual
                         "Gives back list of URLS"
                         (Right
                              [ "https://google.com"
                              , "http://amazon.com"
                              , "https://www.facebook.com"
                              , "http://www.apple.com"
                              ])
                         (links markdown))
              , testCase
                    "complex markdown"
                    (assertEqual
                         "Gives back list of URLS"
                         (Right
                              [ "https://developer.mozilla.org/en-US/docs/Web/API/Window"
                              , "https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle"
                              , "https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect"
                              , "https://developer.mozilla.org/en-US/docs/Learn/HTML/Howto/Use_data_attributes"
                              ])
                         (links complex))
              , testCase
                    "tex"
                    (assertEqual
                         "Gives back list of URLs"
                         (Right
                              [ "https://eloquentjavascript.net/01_values.html"
                              , "http://exploringjs.com/impatient-js/ch_variables-assignment.html"
                              , "https://developers.google.com/web/updates/2015/01/ES6-Template-Strings#string_substitution"
                              , "https://medium.com/dailyjs/the-why-behind-the-wat-an-explanation-of-javascripts-weird-type-system-83b92879a8db"
                              , "https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Building_blocks/conditionals"
                              , "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Loops_and_iteration"
                              , "https://eloquentjavascript.net/02_program_structure.html"
                              , "http://exploringjs.com/impatient-js/ch_control-flow.html"
                              , "https://rosettacode.org/wiki/FizzBuzz"
                              , "https://www.youtube.com/watch?v=QPZ0pIK_wsc"
                              , "https://www.amazon.co.uk/Code-Language-Computer-Hardware-Software/dp/0735611319"
                              , "https://en.wikipedia.org/wiki/Turing_completeness"
                              ])
                         (links tex))
              ]
        , testCase "nothing" (assertEqual "Gives back empty list" (Right []) (links ""))
        ]
