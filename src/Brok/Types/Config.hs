{-# LANGUAGE NoImplicitPrelude #-}

module Brok.Types.Config where

import ClassyPrelude

import Brok.Types.URL (URL)

data Config = Config
    { cache        :: Maybe Integer
    , ignore       :: [URL]
    , interval     :: Integer
    , files        :: [Text]
    , noColor      :: Bool
    , checkCerts   :: Bool
    , onlyFailures :: Bool
    } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig =
    Config
    { cache = Just 84600
    , ignore = []
    , interval = 100
    , files = []
    , noColor = False
    , checkCerts = False
    , onlyFailures = False
    }
