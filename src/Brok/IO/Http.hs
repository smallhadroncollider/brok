{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.Http
    ( check
    ) where

import ClassyPrelude

import Network.HTTP.Simple (HttpException, Request, addRequestHeader, getResponseStatusCode,
                            httpNoBody, parseRequest, setRequestMethod)

import Brok.IO.CLI     (replace)
import Brok.Types.Link

type StatusCode = Either HttpException Int

setHeaders :: Request -> Request
setHeaders = addRequestHeader "User-Agent" "smallhadroncollider/brok"

makeRequest :: ByteString -> URL -> IO StatusCode
makeRequest method url = do
    request <- setHeaders . setRequestMethod method <$> parseRequest (unpack url)
    (getResponseStatusCode <$>) <$> try (httpNoBody request)

tryWithGet :: URL -> StatusCode -> IO StatusCode
tryWithGet url (Right code)
    -- various 400/500 errors mean HEAD support doesn't work 
    -- so try with GET instead 
    | code >= 400 && code /= 404 = makeRequest "GET" url
    | otherwise = return (Right code)
tryWithGet _ sc = return sc

fetch :: URL -> IO StatusCode
fetch url = replace ("Fetching: " ++ url) >> makeRequest "HEAD" url >>= tryWithGet url

codeToResponse :: Link -> StatusCode -> Link
codeToResponse lnk (Right code)
    | code >= 200 && code < 300 = working lnk code
    | otherwise = broken lnk code
codeToResponse lnk (Left _) = failure lnk

check :: Link -> IO Link
check lnk = codeToResponse lnk <$> fetch (getURL lnk)
