{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.Options
    ( options
    ) where

import ClassyPrelude

import Brok.Parser.Links  (url)
import Brok.Parser.Parsec
import Brok.Types.Config
import Brok.Types.Next    (Next (..))

data Option
    = Cache Integer
    | Interval Integer
    | Ignore [Text]
    | Files [Text]

readInt :: String -> String -> Parser Integer
readInt arg value =
    maybe (fail $ "Unable to parse " ++ arg ++ " value") return (readMay value :: Maybe Integer)

cacheP :: Parser Option
cacheP = lexeme $ Cache <$> (text "--cache" *> char '\n' *> many1 digit >>= readInt "cache")

intervalP :: Parser Option
intervalP =
    lexeme $ Interval <$> (text "--interval" *> char '\n' *> many1 digit >>= readInt "interval")

urlP :: Parser Text
urlP = lexeme url

ignoreP :: Parser Option
ignoreP = lexeme $ Ignore <$> (text "--ignore" *> char '\n' *> many1 urlP)

fileP :: Parser Text
fileP = lexeme $ pack <$> many1 (noneOf "\n")

optsToConfig :: [Option] -> Config
optsToConfig = foldl' convert defaultConfig
  where
    convert dc (Cache i)    = dc {cache = i}
    convert dc (Ignore i)   = dc {ignore = i}
    convert dc (Interval i) = dc {interval = i}
    convert dc (Files i)    = dc {files = i}

arguments :: Parser Config
arguments = do
    opts <- optionMaybe $ many1 (try1 cacheP <|> try1 intervalP <|> try1 ignoreP)
    fls <- many1 fileP
    return . optsToConfig $
        case opts of
            Just opts' -> opts' ++ [Files fls]
            Nothing    -> [Files fls]

helpP :: Parser Next
helpP = lexeme $ (try1 (text "--help") <|> text "-h") $> Help

next :: Parser Next
next = try1 helpP <|> (Continue <$> arguments)

-- run parser
options :: [Text] -> Either Text Next
options [] = Left "No files provided"
options content =
    case parse next "" (unlines content) of
        Right c -> Right c
        Left e  -> Left $ tshow e
