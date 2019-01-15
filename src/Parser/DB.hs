{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.DB
    ( db
    ) where

import ClassyPrelude

import Parser.Links  (Link, url)
import Parser.Parsec

line :: Parser (Link, Integer)
line = do
    lnk <- url
    _ <- char ' '
    int <- many1 digit
    _ <- char '\n'
    case readMay int :: Maybe Integer of
        Just i  -> return (lnk, i)
        Nothing -> fail "Unable to parse timestamp"

entries :: Parser [(Link, Integer)]
entries = many1 line

-- run parser
db :: Text -> Either Text [(Link, Integer)]
db "" = Right []
db content =
    case parse entries "" content of
        Right c -> Right c
        Left e  -> Left $ tshow e
