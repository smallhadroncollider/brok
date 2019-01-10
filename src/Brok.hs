{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok
    ( brok
    , links
    ) where

import ClassyPrelude hiding ((<|>))

import Data.Either (fromRight)

import Text.Parsec
import Text.Parsec.Text (Parser)

type Link = Text

type Token = Maybe Link

urlChars :: Parser Char
urlChars = alphaNum <|> oneOf "-._~:/?#%[]@!$&'()*+,;="

url :: Parser Token
url = do
    protocol <- pack <$> string "http"
    secure <- maybe "" singleton <$> optionMaybe (char 's')
    slashes <- pack <$> string "://"
    address <- pack <$> many1 urlChars
    return . Just $ concat [protocol, secure, slashes, address]

noise :: Parser Token
noise = anyToken >> return Nothing

urls :: Parser [Link]
urls = catMaybes <$> many1 (url <|> noise)

links :: Text -> [Link]
links txt = fromRight [] $ parse urls "(unknown)" txt

brok :: IO ()
brok = putStrLn "brok"
