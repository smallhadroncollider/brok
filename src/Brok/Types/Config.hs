{-# LANGUAGE NoImplicitPrelude #-}

module Brok.Types.Config where

import ClassyPrelude

import Brok.Types.Link (URL)

data Config = Config
    { cache    :: Integer
    , ignore   :: [URL]
    , interval :: Integer
    , files    :: [Text]
    } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config {cache = 84600, ignore = [], interval = 20, files = []}
