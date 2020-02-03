module Brok.Types.App
    ( App
    ) where

import ClassyPrelude

import Brok.Types.Config (Config)

type App a = ReaderT Config IO a
