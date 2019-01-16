{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Options where

import ClassyPrelude

import Brok.Parser.Options (options)
import Brok.Types.Config   (Config)

parse :: [Text] -> Either Text Config
parse = options
