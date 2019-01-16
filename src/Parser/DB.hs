{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.DB
    ( db
    ) where

import ClassyPrelude

import Parser.Links  (url)
import Parser.Parsec
import Types.Link

line :: Parser (URL, Integer)
line = do
    lnk <- url
    _ <- char ' '
    int <- many1 digit
    _ <- char '\n'
    case readMay int :: Maybe Integer of
        Just i  -> return (lnk, i)
        Nothing -> fail "Unable to parse timestamp"

entries :: Parser [(URL, Integer)]
entries = many1 line

-- run parser
db :: Text -> Either Text [(URL, Integer)]
db "" = Right []
db content =
    case parse entries "" content of
        Right c -> Right c
        Left e  -> Left $ tshow e
