{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.Http
    ( check
    ) where

import ClassyPrelude

import Control.Concurrent  (threadDelay)
import Network.HTTP.Simple (HttpException, Request, addRequestHeader, getResponseStatusCode,
                            httpNoBody, parseRequest, setRequestMethod)

import Brok.IO.CLI     (replace)
import Brok.Types.Link

type StatusCode = Either HttpException Int

setHeaders :: Request -> Request
setHeaders = addRequestHeader "User-Agent" "smallhadroncollider/brok"

makeRequest :: Integer -> ByteString -> URL -> IO StatusCode
makeRequest delay method url = do
    request <- setHeaders . setRequestMethod method <$> parseRequest (unpack url)
    threadDelay (fromIntegral delay * 1000) -- wait for a little while
    (getResponseStatusCode <$>) <$> try (httpNoBody request)

tryWithGet :: Integer -> URL -> StatusCode -> IO StatusCode
tryWithGet delay url (Right code)
    -- various 400/500 errors mean HEAD support doesn't work 
    -- so try with GET instead 
    | code >= 400 && code /= 404 = makeRequest delay "GET" url
    | otherwise = return (Right code)
tryWithGet _ _ sc = return sc

fetch :: Integer -> URL -> IO StatusCode
fetch delay url =
    replace ("Fetching: " ++ url) >> makeRequest delay "HEAD" url >>= tryWithGet delay url

codeToResponse :: Link -> StatusCode -> Link
codeToResponse lnk (Right code)
    | code >= 200 && code < 300 = working lnk code
    | otherwise = broken lnk code
codeToResponse lnk (Left _) = failure lnk

check :: Integer -> Link -> IO Link
check delay lnk = codeToResponse lnk <$> fetch delay (getURL lnk)
