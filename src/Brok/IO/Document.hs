{-# LANGUAGE NoImplicitPrelude #-}

module Brok.IO.Document
    ( TFilePath
    , readContent
    ) where

import ClassyPrelude

import System.Directory (doesFileExist)

import Brok.Types.Result

readContent :: Result -> IO Result
readContent result = do
    let path = getPath result
    let filepath = unpack path
    exists <- doesFileExist filepath
    if exists
        then setContent result . decodeUtf8 <$> readFile filepath
        else return $ notFound result
