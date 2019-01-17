{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.Options
    ( options
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Brok.Parser.Attoparsec
import Brok.Parser.Links      (url)
import Brok.Types.Config
import Brok.Types.Next        (Next (..))

data Option
    = Cache Integer
    | Interval Integer
    | Ignore [Text]
    | Files [Text]

readInt :: String -> String -> Parser Integer
readInt arg value =
    maybe (fail $ "Unable to parse " ++ arg ++ " value") return (readMay value :: Maybe Integer)

cacheP :: Parser Option
cacheP = lexeme $ Cache <$> (string "--cache" *> char '\n' *> many1 digit >>= readInt "cache")

intervalP :: Parser Option
intervalP =
    lexeme $ Interval <$> (string "--interval" *> char '\n' *> many1 digit >>= readInt "interval")

urlP :: Parser Text
urlP = lexeme url

ignoreP :: Parser Option
ignoreP = lexeme $ Ignore <$> (string "--ignore" *> char '\n' *> many1 urlP)

fileP :: Parser Text
fileP = lexeme $ pack <$> many1 (notChar '\n')

optsToConfig :: [Option] -> Config
optsToConfig = foldl' convert defaultConfig
  where
    convert dc (Cache i)    = dc {cache = i}
    convert dc (Ignore i)   = dc {ignore = i}
    convert dc (Interval i) = dc {interval = i}
    convert dc (Files i)    = dc {files = i}

arguments :: Parser Config
arguments = do
    opts <- many' (cacheP <|> intervalP <|> ignoreP)
    fls <- many1 fileP
    return . optsToConfig $ opts ++ [Files fls]

helpP :: Parser Next
helpP = lexeme $ (string "--help" <|> string "-h") $> Help

next :: Parser Next
next = helpP <|> (Continue <$> arguments)

-- run parser
options :: [Text] -> Either Text Next
options [] = Left "No files provided"
options content =
    case parseOnly next (unlines content) of
        Right c -> Right c
        Left e  -> Left $ tshow e
