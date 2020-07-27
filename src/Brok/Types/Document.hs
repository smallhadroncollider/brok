{-# LANGUAGE NoImplicitPrelude #-}

module Brok.Types.Document where

import ClassyPrelude

import Brok.IO.Http      (check)
import Brok.Parser.Links (links)
import Brok.Types.Brok   (Brok)
import Brok.Types.Link
import Brok.Types.URL    (URL)

type TFilePath = Text

type Error = Text

data Phase
    = Content Text
    | NotFound
    | ParseError Error
    | Links [Link]
    deriving (Show, Eq)

data Document =
    Document TFilePath
             Phase
    deriving (Show, Eq)

getPath :: Document -> TFilePath
getPath (Document path _) = path

notFound :: TFilePath -> Document
notFound path = Document path NotFound

withContent :: TFilePath -> Text -> Document
withContent path text = Document path (Content text)

parseLinks :: Document -> Document
parseLinks (Document path (Content text)) =
    case links text of
        Left err   -> Document path (ParseError err)
        Right lnks -> Document path (Links $ urlToLink <$> lnks)
parseLinks result = result

findLinks :: ([URL] -> Link -> Link) -> [URL] -> Document -> Document
findLinks fn urls (Document path (Links lnks)) = Document path (Links $ fn urls <$> lnks)
findLinks _ _ result                           = result

cachedLinks :: [URL] -> Document -> Document
cachedLinks = findLinks cachedLink

ignoredLinks :: [URL] -> Document -> Document
ignoredLinks = findLinks ignoredLink

checkLinks :: Integer -> Document -> Brok Document
checkLinks interval (Document path (Links lnks)) =
    Document path . Links <$> traverse (check interval) lnks
checkLinks _ result = pure result

justLinks :: Document -> [Link]
justLinks (Document _ (Links lnks)) = lnks
justLinks _                         = []
