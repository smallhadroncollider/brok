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

import Brok.IO.CLI       (replace)
import Brok.Types.Brok   (Brok, appConfig, appTLSManager)
import Brok.Types.Config (interval)
import Brok.Types.Link
import Brok.Types.URL    (URL)

type StatusCode = Either HttpException Int

mkManager :: Bool -> IO Manager
mkManager checkCerts = do
    let tls = TLSSettingsSimple (not checkCerts) False False
    let settings = mkManagerSettings tls Nothing
    newManager settings

setHeaders :: Request -> Request
setHeaders = addRequestHeader "User-Agent" "smallhadroncollider/brok"

makeRequest :: ByteString -> URL -> Brok StatusCode
makeRequest method url = do
    manager <- asks appTLSManager
    delay <- interval <$> asks appConfig
    lift . try $ do
        request <- setHeaders . setRequestMethod method <$> parseRequest (unpack url)
        threadDelay (fromIntegral delay * 1000) -- wait for a little while
        getResponseStatusCode <$> httpNoBody request manager

tryWithGet :: URL -> StatusCode -> Brok StatusCode
tryWithGet url (Right code)
    | code >= 400 = makeRequest "GET" url
    | otherwise = pure (Right code)
tryWithGet url (Left (HttpExceptionRequest _ (InternalException _))) = makeRequest "GET" url
tryWithGet url (Left _) = makeRequest "GET" url

fetch :: URL -> Brok StatusCode
fetch url = replace ("Fetching: " <> url) >> makeRequest "HEAD" url >>= tryWithGet url

codeToResponse :: Link -> StatusCode -> Link
codeToResponse lnk (Right code)
    | code >= 200 && code < 300 = working lnk code
    | otherwise = broken lnk code
codeToResponse lnk (Left (HttpExceptionRequest _ _)) = failure lnk
codeToResponse lnk (Left (InvalidUrlException _ _)) = invalid lnk

check :: Link -> Brok Link
check lnk = codeToResponse lnk <$> fetch (getURL lnk)
