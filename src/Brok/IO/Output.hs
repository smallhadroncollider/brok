{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.Output
    ( output
    ) where

import ClassyPrelude

import Brok.IO.CLI
import Brok.Types.Brok     (Brok)
import Brok.Types.Document
import Brok.Types.Link

-- output
linkOutput :: Link -> Brok ()
linkOutput (Link url UnresolvedLink)    = splitErr "- Failed (unknown)" url
linkOutput (Link url Ignored)           = mehssage $ "- Ignored: " <> url
linkOutput (Link url Cached)            = splitOut "- OK (cached)" url
linkOutput (Link url (Working code))    = splitOut ("- OK (" <> tshow code <> ")") url
linkOutput (Link url (Broken code))     = splitErr ("- Failed (" <> tshow code <> ")") url
linkOutput (Link url ConnectionFailure) = splitErr "- Could not connect" url
linkOutput (Link url InvalidURL)        = splitErr "- Invalid URL" url

statusError :: Link -> Bool
statusError (Link _ (Working _)) = False
statusError (Link _ Cached)      = False
statusError (Link _ Ignored)     = False
statusError _                    = True

outputPath :: TFilePath -> Text
outputPath path = concat ["\n", "[", path, "]"]

outputMap :: Bool -> Document -> Brok Bool
outputMap _ (Document path NotFound) = do
    errorMessage $ outputPath path
    errorMessage "  - File not found"
    pure True
outputMap _ (Document path (ParseError err)) = do
    errorMessage $ outputPath path
    errorMessage "  - Parse error:"
    errorMessage err
    pure True
outputMap onlyFailures (Document path (Links links)) = do
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
    pure anyErrs
outputMap _ _ = pure False

output :: Bool -> [Document] -> Brok Bool
output onlyFailures results = do
    errs <- traverse (outputMap onlyFailures) results
    let anyErrs = foldl' (||) False errs
    when (not anyErrs && onlyFailures) $ successMessage "All links working"
    pure anyErrs
