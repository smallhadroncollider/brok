{-# LANGUAGE NoImplicitPrelude #-}

module Brok.IO.Document
    ( TFilePath
    , readContent
    ) where

import ClassyPrelude

import System.Directory (doesFileExist)

import Brok.Types.Brok   (Brok)
import Brok.Types.Result

readContent :: Result -> Brok Result
readContent result = do
    let path = getPath result
    let filepath = unpack path
    exists <- lift $ doesFileExist filepath
    if exists
        then setContent result . decodeUtf8 <$> readFile filepath
        else pure $ notFound result
