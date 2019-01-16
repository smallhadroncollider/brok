{-# LANGUAGE NoImplicitPrelude #-}

module Types.Link where

import ClassyPrelude

type URL = Text

data LinkType
    = BareLink
    | Cached
    | Working Int
    | Broken Int
    | ConnectionFailure
    deriving (Show, Eq)

data Link =
    Link URL
         LinkType
    deriving (Show, Eq)

urlToLink :: URL -> Link
urlToLink url = Link url BareLink

getURL :: Link -> URL
getURL (Link url _) = url

working :: Link -> Int -> Link
working (Link url BareLink) code = Link url (Working code)
working lnk _                    = lnk

broken :: Link -> Int -> Link
broken (Link url BareLink) code = Link url (Broken code)
broken lnk _                    = lnk

failure :: Link -> Link
failure (Link url BareLink) = Link url ConnectionFailure
failure lnk                 = lnk

cachedLink :: [URL] -> Link -> Link
cachedLink cached (Link url BareLink) =
    case find (== url) cached of
        Just _  -> Link url Cached
        Nothing -> Link url BareLink
cachedLink _ lnk = lnk

isSuccess :: Link -> Bool
isSuccess (Link _ (Working _)) = True
isSuccess _                    = False

lmap :: (Link -> IO Link) -> Link -> IO Link
lmap fn (Link url BareLink) = fn (Link url BareLink)
lmap _ lnk                  = return lnk
