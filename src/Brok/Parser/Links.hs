{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.Links
    ( links
    , url
    ) where

import ClassyPrelude

import Data.List (nub)

import Brok.Parser.Parsec
import Brok.Types.Link    (URL)

type Token = Maybe URL

-- parentheses
parens :: Parser Text -> Parser Text
parens parser = surround '(' ')' parser <|> surround '[' ']' parser

-- urls
urlChar :: Parser Char
urlChar = alphaNum <|> oneOf "-._~:/?#%@!$&*+,;="

urlChars :: Parser Text
urlChars = concat <$> many1 (parens urlChars <|> (pack <$> many1 urlChar))

url :: Parser Text
url = concat4 <$> text "http" <*> chopt 's' <*> text "://" <*> urlChars

noise :: Parser Token
noise = anyToken >> return Nothing

urls :: Parser [URL]
urls = nub . catMaybes <$> many1 ((Just <$> try1 url) <|> noise)

-- run parser
links :: Text -> Either Text [URL]
links "" = Right []
links content =
    case parse urls "" content of
        Right c -> Right c
        Left e  -> Left $ tshow e
