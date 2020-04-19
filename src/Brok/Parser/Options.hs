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
    = Cache (Maybe Integer)
    | Interval Integer
    | Ignore [Text]
    | Files [Text]
    | NoColor
    | CheckCerts
    | OnlyFailures

onlyFailuresP :: Parser Option
onlyFailuresP = lexeme $ string "--only-failures" $> OnlyFailures

noColorP :: Parser Option
noColorP = lexeme $ string "--no-color" $> NoColor

checkCertsP :: Parser Option
checkCertsP = lexeme $ string "--check-certs" $> CheckCerts

noCacheP :: Parser Option
noCacheP = lexeme $ string "--no-cache" $> Cache Nothing

cacheP :: Parser Option
cacheP = lexeme $ Cache . Just <$> (string "--cache" *> char '\n' *> decimal)

intervalP :: Parser Option
intervalP = lexeme $ Interval <$> (string "--interval" *> char '\n' *> decimal)

urlP :: Parser Text
urlP = lexeme url

ignoreP :: Parser Option
ignoreP = lexeme $ Ignore <$> (string "--ignore" *> char '\n' *> many1 urlP)

fileP :: Parser Text
fileP = lexeme $ manyChars (notChar '\n')

optsToConfig :: [Option] -> Config
optsToConfig = foldl' convert defaultConfig
  where
    convert dc (Cache i)    = dc {cache = i}
    convert dc (Ignore i)   = dc {ignore = i}
    convert dc (Interval i) = dc {interval = i}
    convert dc (Files i)    = dc {files = i}
    convert dc NoColor      = dc {noColor = True}
    convert dc CheckCerts   = dc {checkCerts = True}
    convert dc OnlyFailures = dc {onlyFailures = True}

arguments :: Parser Config
arguments = do
    opts <-
        many'
            (noCacheP <|> cacheP <|> intervalP <|> ignoreP <|> noColorP <|> checkCertsP <|>
             onlyFailuresP)
    fls <- many1 fileP
    return . optsToConfig $ opts ++ [Files fls]

helpP :: Parser Next
helpP = lexeme $ (string "--help" <|> string "-h") $> Help

versionP :: Parser Next
versionP = lexeme $ (string "--version" <|> string "-v") $> Version

next :: Parser Next
next = helpP <|> versionP <|> (Continue <$> arguments)

-- run parser
options :: [Text] -> Either Text Next
options [] = Left "No files provided"
options content =
    case parseOnly next (unlines content) of
        Right c -> Right c
        Left e  -> Left $ tshow e
