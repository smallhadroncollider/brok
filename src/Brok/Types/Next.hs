{-# LANGUAGE NoImplicitPrelude #-}

module Brok.Types.Next where

import ClassyPrelude

import Brok.Types.Config (Config)

data Next
    = Continue Config
    | Help
    | Version
    deriving (Show, Eq)
