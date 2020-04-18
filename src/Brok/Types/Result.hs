{-# LANGUAGE NoImplicitPrelude #-}

module Brok.Types.Result where

import ClassyPrelude

import Brok.Types.Brok  (Brok)
import Brok.Types.Link
import Brok.Types.URL  (URL)

type TFilePath = Text

type Error = Text

data ResultType
    = BareResult
    | Content Text
    | NotFound
    | ParseError Error
    | Links [Link]
    deriving (Show, Eq)

data Result =
    Result TFilePath
           ResultType
    deriving (Show, Eq)

pathToResult :: TFilePath -> Result
pathToResult path = Result path BareResult

getPath :: Result -> TFilePath
getPath (Result path _) = path

notFound :: Result -> Result
notFound (Result path BareResult) = Result path NotFound
notFound result                   = result

setContent :: Result -> Text -> Result
setContent (Result path BareResult) text = Result path (Content text)
setContent result _                      = result

parseLinks :: (Text -> Either Text [URL]) -> Result -> Result
parseLinks fn (Result path (Content text)) =
    case fn text of
        Left err    -> Result path (ParseError err)
        Right links -> Result path (Links $ urlToLink <$> links)
parseLinks _ result = result

findLinks :: ([URL] -> Link -> Link) -> [URL] -> Result -> Result
findLinks fn urls (Result path (Links links)) = Result path (Links $ fn urls <$> links)
findLinks _ _ result                          = result

cachedLinks :: [URL] -> Result -> Result
cachedLinks = findLinks cachedLink

ignoredLinks :: [URL] -> Result -> Result
ignoredLinks = findLinks ignoredLink

linkIOMap :: (Link -> Brok Link) -> Result -> Brok Result
linkIOMap fn (Result path (Links links)) = Result path . Links <$> traverse (lmap fn) links
linkIOMap _ result                       = pure result

justLinks :: Result -> [Link]
justLinks (Result _ (Links links)) = links
justLinks _                        = []
