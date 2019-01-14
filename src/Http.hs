{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Http
    ( LinkStatus(..)
    , check
    ) where

import ClassyPrelude

import Network.HTTP.Simple (HttpException, Request, addRequestHeader, getResponseStatusCode,
                            httpNoBody, parseRequest, setRequestMethod)

import Parser.Links (Link)

data LinkStatus
    = Working
    | Broken Int
    | ConnectionFailure
    deriving (Show, Eq)

type StatusCode = Either HttpException Int

setHeaders :: Request -> Request
setHeaders = addRequestHeader "User-Agent" "smallhadroncollider/brok"

makeRequest :: ByteString -> Link -> IO StatusCode
makeRequest method url = do
    request <- setHeaders . setRequestMethod method <$> parseRequest (unpack url)
    (getResponseStatusCode <$>) <$> try (httpNoBody request)

tryWithGet :: Link -> StatusCode -> IO StatusCode
tryWithGet url (Right code)
    -- various 400/500 errors mean HEAD support doesn't work 
    -- so try with GET instead 
    | code >= 400 && code /= 404 = makeRequest "GET" url
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
