module Brok.Types.Brok
    ( Brok
    , appConfig
    , appTLSManager
    , mkApp
    ) where

import ClassyPrelude

import Brok.Types.Config   (Config)
import Network.HTTP.Client (Manager)

data App = App
    { appConfig     :: Config
    , appTLSManager :: Manager
    }

mkApp :: Config -> Manager -> App
mkApp config manager = App config manager

type Brok a = ReaderT App IO a
