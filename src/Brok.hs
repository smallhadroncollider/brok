{-# LANGUAGE NoImplicitPrelude #-}

module Brok
    ( brok
    ) where

import ClassyPrelude

import System.Exit (exitFailure, exitSuccess)

import Http         (check)
import IO.Document  (readContent)
import IO.Output    (output)
import Parser.Links (links)

joinEither :: Either a (Either a b) -> Either a b
joinEither (Left a)          = Left a
joinEither (Right (Left a))  = Left a
joinEither (Right (Right b)) = Right b

-- entry point
brok :: IO ()
brok
    -- get files from command line
 = do
    files <- getArgs
    -- read files
    content <- sequence (readContent <$> files)
    -- find links in each file
    let parsed = (joinEither . (links <$>) <$>) <$> content
    -- check links in each file
    checked <- sequence (sequence . (sequence . (check <$>) <$>) <$> parsed)
    -- display results
    anyErrors <- sequence $ output <$> checked
    -- exit with appropriate status code
    if foldl' (||) False anyErrors
        then void exitFailure
        else void exitSuccess
