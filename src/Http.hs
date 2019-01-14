{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Http
    ( LinkStatus(..)
    , check
    ) where

import ClassyPrelude

import Network.HTTP.Simple (HttpException, getResponseStatusCode, httpNoBody, parseRequest,
                            setRequestMethod)

import Parser.Links (Link)

data LinkStatus
    = Working
    | Broken Int
    | ConnectionFailure

type StatusCode = Either HttpException Int

makeRequest :: ByteString -> Link -> IO StatusCode
makeRequest method url = do
    request <- setRequestMethod method <$> parseRequest (unpack url)
    (getResponseStatusCode <$>) <$> try (httpNoBody request)

tryWithGet :: Link -> StatusCode -> IO StatusCode
tryWithGet url (Right code)
    -- if 405 or 500 then try a GET request instead
    -- not all servers support HEAD
    | code == 405 || code >= 500 = makeRequest "GET" url
    | otherwise = return (Right code)
tryWithGet _ sc = return sc

fetch :: Link -> IO StatusCode
fetch url = makeRequest "HEAD" url >>= tryWithGet url

codeToResponse :: Link -> StatusCode -> (Link, LinkStatus)
codeToResponse url (Right code)
    | code >= 200 && code < 300 = (url, Working)
    | otherwise = (url, Broken code)
codeToResponse url (Left _) = (url, ConnectionFailure)

broken :: Link -> IO (Link, LinkStatus)
broken url = codeToResponse url <$> fetch url

check :: [Link] -> IO [(Link, LinkStatus)]
check links = sequence (broken <$> links)
