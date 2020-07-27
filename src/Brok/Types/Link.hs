{-# LANGUAGE NoImplicitPrelude #-}

module Brok.Types.Link where

import ClassyPrelude

import Brok.Types.URL (URL)

data LinkType
    = BareLink
    | Cached
    | Ignored
    | Working Int
    | Broken Int
    | InvalidURL
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

invalid :: Link -> Link
invalid (Link url BareLink) = Link url InvalidURL
invalid lnk                 = lnk

findLink :: LinkType -> (URL -> URL -> Bool) -> [URL] -> Link -> Link
findLink lType fn urls (Link url BareLink) =
    case find (fn url) urls of
        Just _  -> Link url lType
        Nothing -> Link url BareLink
findLink _ _ _ lnk = lnk

cachedLink :: [URL] -> Link -> Link
cachedLink = findLink Cached (==)

ignoredLink :: [URL] -> Link -> Link
ignoredLink = findLink Ignored (flip isPrefixOf)

isSuccess :: Link -> Bool
isSuccess (Link _ (Working _)) = True
isSuccess _                    = False
