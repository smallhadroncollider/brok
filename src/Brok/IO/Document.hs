{-# LANGUAGE NoImplicitPrelude #-}

module Brok.IO.Document
    ( TFilePath
    , readContent
    ) where

import ClassyPrelude

import System.Directory (doesFileExist)

import Brok.Types.Brok     (Brok)
import Brok.Types.Document

readContent :: TFilePath -> Brok Document
readContent path = do
    let filepath = unpack path
    exists <- lift $ doesFileExist filepath
    if exists
        then withContent path . decodeUtf8 <$> readFile filepath
        else pure $ notFound path
