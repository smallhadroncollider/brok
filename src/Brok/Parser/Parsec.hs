{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.Parsec
    ( Parser
    , alphaNum
    , anyToken
    , char
    , digit
    , many1
    , oneOf
    , optionMaybe
    , noneOf
    , parse
    , string
    , try1
    , text
    , tchar
    , chopt
    , surround
    , concat3
    , concat4
    , spaces
    ) where

import ClassyPrelude hiding (try)

import Text.Parsec      (alphaNum, anyToken, char, digit, many1, noneOf, oneOf, optionMaybe, parse,
                         spaces, string, try)
import Text.Parsec.Text (Parser)

try1 :: Parser a -> Parser a
try1 = try

text :: String -> Parser Text
text str = pack <$> string str

tchar :: Char -> Parser Text
tchar ch = singleton <$> char ch

chopt :: Char -> Parser Text
chopt ch = maybe "" singleton <$> optionMaybe (char ch)

surround :: Char -> Char -> Parser Text -> Parser Text
surround open close parser = concat3 <$> tchar open <*> parser <*> tchar close

concat3 :: (Monoid a) => a -> a -> a -> a
concat3 t1 t2 t3 = concat [t1, t2, t3]

concat4 :: (Monoid a) => a -> a -> a -> a -> a
concat4 t1 t2 t3 t4 = concat [t1, t2, t3, t4]
