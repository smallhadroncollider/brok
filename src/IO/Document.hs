{-# LANGUAGE NoImplicitPrelude #-}

module IO.Document
    ( TFilePath
    , readContent
    ) where

import ClassyPrelude

import System.Directory (doesFileExist)

type TFilePath = Text

readIfTrue :: FilePath -> Bool -> IO (Maybe Text)
readIfTrue path True = Just <$> (decodeUtf8 <$> readFile path)
readIfTrue _ False   = return Nothing

readContent :: TFilePath -> IO (TFilePath, Maybe Text)
readContent path = do
    let filepath = unpack path
    status <- doesFileExist filepath >>= readIfTrue filepath
    return (path, status)
