{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok
    ( brok
    , links
    ) where

import ClassyPrelude

import Data.Either (fromRight)

import Text.Parsec      (alphaNum, anyToken, char, many1, oneOf, optionMaybe, parse, string)
import Text.Parsec.Text (Parser)

type Link = Text

type Token = Maybe Link

surround :: Char -> Char -> Parser Text -> Parser Text
surround open close parser = do
    o <- singleton <$> char open
    fragment <- parser
    c <- singleton <$> char close
    return $ o ++ fragment ++ c

parens :: Parser Text -> Parser Text
parens parser = surround '(' ')' parser <|> surround '[' ']' parser

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
urls = catMaybes <$> many1 (url <|> noise)

links :: FilePath -> Text -> [Link]
links path txt = fromRight [] $ parse urls path txt

brok :: IO ()
brok = putStrLn "brok"
