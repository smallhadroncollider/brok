{-# LANGUAGE NoImplicitPrelude #-}

module IO.Document
    ( TFilePath
    , readContent
    ) where

import ClassyPrelude

import System.Directory (doesFileExist)

type TFilePath = Text

readIfTrue :: FilePath -> Bool -> IO (Either Text Text)
readIfTrue path True = Right <$> (decodeUtf8 <$> readFile path)
readIfTrue _ False   = return $ Left "File not found"

readContent :: TFilePath -> IO (TFilePath, Either Text Text)
readContent path = do
    let filepath = unpack path
    status <- doesFileExist filepath >>= readIfTrue filepath
    return (path, status)
