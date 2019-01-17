{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.Links
    ( links
    , url
    ) where

import ClassyPrelude

import Data.Attoparsec.Text
import Data.List            (nub)

import Brok.Parser.Attoparsec
import Brok.Types.Link        (URL)

type Token = Maybe URL

-- parentheses
parens :: Parser Text -> Parser Text
parens parser = surround '(' ')' parser <|> surround '[' ']' parser

-- urls
urlChar :: Parser Char
urlChar = digit <|> letter <|> choice (char <$> "-._~:/?#%@!$&*+,;=")

urlChars :: Parser Text
urlChars = concat <$> many1 (parens urlChars <|> (pack <$> many1 urlChar))

url :: Parser Text
url = concat4 <$> string "http" <*> chopt 's' <*> string "://" <*> urlChars

noise :: Parser Token
noise = anyChar >> return Nothing

urls :: Parser [URL]
urls = nub . catMaybes <$> many1 ((Just <$> url) <|> noise)

-- run parser
links :: Text -> Either Text [URL]
links "" = Right []
links content =
    case parseOnly urls content of
        Right c -> Right c
        Left e  -> Left $ tshow e
