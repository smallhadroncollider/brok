{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.Http
    ( check
    , mkManager
    ) where

import ClassyPrelude

import Control.Concurrent      (threadDelay)
import Network.Connection      (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client     (HttpExceptionContent (InternalException), Manager, httpNoBody,
                                newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Simple     (HttpException, HttpException (..), Request, addRequestHeader,
                                getResponseStatusCode, parseRequest, setRequestMethod)

import Brok.IO.CLI     (replace)
import Brok.Types.Brok (Brok, appTLSManager)
import Brok.Types.Link
import Brok.Types.URL  (URL)

type StatusCode = Either HttpException Int

mkManager :: Bool -> IO Manager
mkManager checkCerts = do
    let tls = TLSSettingsSimple (not checkCerts) False False
    let settings = mkManagerSettings tls Nothing
    newManager settings

setHeaders :: Request -> Request
setHeaders = addRequestHeader "User-Agent" "smallhadroncollider/brok"

makeRequest :: Integer -> ByteString -> URL -> Brok StatusCode
makeRequest delay method url = do
    manager <- asks appTLSManager
    lift . try $ do
        request <- setHeaders . setRequestMethod method <$> parseRequest (unpack url)
        threadDelay (fromIntegral delay * 1000) -- wait for a little while
        getResponseStatusCode <$> httpNoBody request manager

tryWithGet :: Integer -> URL -> StatusCode -> Brok StatusCode
tryWithGet delay url (Right code)
    | code >= 400 = makeRequest delay "GET" url
    | otherwise = pure (Right code)
tryWithGet delay url (Left (HttpExceptionRequest _ (InternalException _))) =
    makeRequest delay "GET" url
tryWithGet delay url (Left _) = makeRequest delay "GET" url

fetch :: Integer -> URL -> Brok StatusCode
fetch delay url =
    replace ("Fetching: " <> url) >> makeRequest delay "HEAD" url >>= tryWithGet delay url

codeToResponse :: Link -> StatusCode -> Link
codeToResponse lnk (Right code)
    | code >= 200 && code < 300 = working lnk code
    | otherwise = broken lnk code
codeToResponse lnk (Left (HttpExceptionRequest _ _)) = failure lnk
codeToResponse lnk (Left (InvalidUrlException _ _)) = invalid lnk

check :: Integer -> Link -> Brok Link
check delay lnk = codeToResponse lnk <$> fetch delay (getURL lnk)
