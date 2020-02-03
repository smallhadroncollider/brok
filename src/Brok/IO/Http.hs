{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.Http
    ( check
    ) where

import ClassyPrelude

import Control.Concurrent  (threadDelay)
import Network.HTTP.Simple (HttpException, HttpException (..), Request, addRequestHeader,
                            getResponseStatusCode, httpNoBody, parseRequest, setRequestMethod)

import Brok.IO.CLI     (replace)
import Brok.Types.App  (App)
import Brok.Types.Link
import Brok.Types.URL  (URL)

type StatusCode = Either HttpException Int

setHeaders :: Request -> Request
setHeaders = addRequestHeader "User-Agent" "smallhadroncollider/brok"

makeRequest :: Integer -> ByteString -> URL -> App StatusCode
makeRequest delay method url =
    lift . try $ do
        request <- setHeaders . setRequestMethod method <$> parseRequest (unpack url)
        threadDelay (fromIntegral delay * 1000) -- wait for a little while
        getResponseStatusCode <$> httpNoBody request

tryWithGet :: Integer -> URL -> StatusCode -> App StatusCode
tryWithGet delay url (Right code)
    | code >= 400 = makeRequest delay "GET" url
    | otherwise = return (Right code)
tryWithGet delay url (Left _) = makeRequest delay "GET" url

fetch :: Integer -> URL -> App StatusCode
fetch delay url =
    replace ("Fetching: " ++ url) >> makeRequest delay "HEAD" url >>= tryWithGet delay url

codeToResponse :: Link -> StatusCode -> Link
codeToResponse lnk (Right code)
    | code >= 200 && code < 300 = working lnk code
    | otherwise = broken lnk code
codeToResponse lnk (Left (HttpExceptionRequest _ _)) = failure lnk
codeToResponse lnk (Left (InvalidUrlException _ _)) = invalid lnk

check :: Integer -> Link -> App Link
check delay lnk = codeToResponse lnk <$> fetch delay (getURL lnk)
