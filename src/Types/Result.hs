{-# LANGUAGE NoImplicitPrelude #-}

module Types.Result where

import ClassyPrelude

import Types.Link

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

cachedLinks :: [URL] -> Result -> Result
cachedLinks cached (Result path (Links links)) = Result path (Links $ cachedLink cached <$> links)
cachedLinks _ result                           = result

linkIOMap :: (Link -> IO Link) -> Result -> IO Result
linkIOMap fn (Result path (Links links)) = Result path . Links <$> sequence (lmap fn <$> links)
linkIOMap _ result                       = return result

justLinks :: Result -> [Link]
justLinks (Result _ (Links links)) = links
justLinks _                        = []
