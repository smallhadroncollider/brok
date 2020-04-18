module Brok.Types.Brok
    ( Brok
    , appConfig
    , mkApp
    ) where

import ClassyPrelude

import Brok.Types.Config (Config)

data App = App
    { appConfig :: Config
    }

mkApp :: Config -> App
mkApp config = App config

type Brok a = ReaderT App IO a
