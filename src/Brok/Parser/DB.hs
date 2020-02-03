{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.DB
    ( db
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Brok.Parser.Links (url)
import Brok.Types.URL    (URL)

line :: Parser (URL, Integer)
line = (,) <$> (url <* char ' ') <*> (decimal <* endOfLine)

entries :: Parser [(URL, Integer)]
entries = many1 line

-- run parser
db :: Text -> Either Text [(URL, Integer)]
db "" = Right []
db content =
    case parseOnly entries content of
        Right c -> Right c
        Left e  -> Left $ tshow e
