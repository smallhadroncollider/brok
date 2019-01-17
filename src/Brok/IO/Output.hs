{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.Output
    ( output
    ) where

import ClassyPrelude

import Brok.IO.CLI
import Brok.Types.Link
import Brok.Types.Result

-- output
linkOutput :: Link -> IO ()
linkOutput (Link url BareLink)          = splitErr "- Failed (unknown)" url
linkOutput (Link url Ignored)           = mehssage $ "- Ignored: " ++ url
linkOutput (Link url Cached)            = splitOut "- OK (cached)" url
linkOutput (Link url (Working code))    = splitOut ("- OK (" ++ tshow code ++ ")") url
linkOutput (Link url (Broken code))     = splitErr ("- Failed (" ++ tshow code ++ ")") url
linkOutput (Link url ConnectionFailure) = splitErr "- Could not connect" url
linkOutput (Link url InvalidURL)        = splitErr "- Invalid URL" url

statusError :: Link -> Bool
statusError (Link _ (Working _)) = False
statusError (Link _ Cached)      = False
statusError (Link _ Ignored)     = False
statusError _                    = True

countErrors :: [Link] -> Int
countErrors statuses = length $ filter statusError statuses

outputPath :: TFilePath -> Text
outputPath path = concat ["\n", "[", path, "]"]

output :: Result -> IO Bool
output (Result path NotFound) = do
    errorMessage $ outputPath path
    errorMessage "  - File not found"
    return True
output (Result path (ParseError err)) = do
    errorMessage $ outputPath path
    errorMessage "  - Parse error:"
    errorMessage err
    return True
output (Result path (Links links)) = do
    let errs = countErrors links /= 0
    if errs
        then errorMessage $ outputPath path
        else message $ outputPath path
    if not (null links)
        then sequence_ $ linkOutput <$> links
        else putStrLn "- No links found in file"
    return errs
output _ = return False
