{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.Output
    ( output
    ) where

import ClassyPrelude

import Brok.IO.CLI
import Brok.Types.App    (App)
import Brok.Types.Link
import Brok.Types.Result

-- output
linkOutput :: Link -> App ()
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

outputPath :: TFilePath -> Text
outputPath path = concat ["\n", "[", path, "]"]

outputMap :: Bool -> Result -> App Bool
outputMap _ (Result path NotFound) = do
    errorMessage $ outputPath path
    errorMessage "  - File not found"
    return True
outputMap _ (Result path (ParseError err)) = do
    errorMessage $ outputPath path
    errorMessage "  - Parse error:"
    errorMessage err
    return True
outputMap onlyFailures (Result path (Links links)) = do
    let errs = filter statusError links
    let anyErrs = not (null errs)
    if anyErrs
        then do
            errorMessage $ outputPath path
            traverse_
                linkOutput
                (if onlyFailures
                     then errs
                     else links)
        else unless onlyFailures $ do
                 message $ outputPath path
                 if not (null links)
                     then traverse_ linkOutput links
                     else putStrLn "- No links found in file"
    return anyErrs
outputMap _ _ = return False

output :: Bool -> [Result] -> App Bool
output onlyFailures results = do
    errs <- traverse (outputMap onlyFailures) results
    let anyErrs = foldl' (||) False errs
    when (not anyErrs && onlyFailures) $ successMessage "All links working"
    return anyErrs
