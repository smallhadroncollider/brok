{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LinkParser
    ( Link
    , links
    ) where

import ClassyPrelude hiding (try)

import Text.Parsec      (alphaNum, anyToken, char, many1, oneOf, optionMaybe, parse, string, try)
import Text.Parsec.Text (Parser)

type Link = Text

type Token = Maybe Link

-- parentheses
surround :: Char -> Char -> Parser Text -> Parser Text
surround open close parser = do
    o <- singleton <$> char open
    fragment <- parser
    c <- singleton <$> char close
    return $ o ++ fragment ++ c

parens :: Parser Text -> Parser Text
parens parser = surround '(' ')' parser <|> surround '[' ']' parser

-- urls
urlChar :: Parser Char
urlChar = alphaNum <|> oneOf "-._~:/?#%@!$&*+,;="

urlChars :: Parser Text
urlChars = concat <$> many1 (parens urlChars <|> (pack <$> many1 urlChar))

url :: Parser Token
url = do
    protocol <- pack <$> string "http"
    secure <- maybe "" singleton <$> optionMaybe (char 's')
    slashes <- pack <$> string "://"
    address <- urlChars
    return . Just $ concat [protocol, secure, slashes, address]

noise :: Parser Token
noise = anyToken >> return Nothing

urls :: Parser [Link]
urls = catMaybes <$> many1 (try url <|> noise)

-- run parser
links :: FilePath -> Text -> Either Text [Link]
links path content =
    case parse urls path content of
        Right c -> Right c
        Left e  -> Left $ tshow e
