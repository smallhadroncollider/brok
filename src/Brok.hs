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

brok :: IO ()
brok
    -- get files from command line
 = do
    files <- getArgs
    -- read files
    content <- sequence (readContent <$> files)
    -- find links in each file
    let parsed = ((links <$>) <$>) <$> content
    -- check links in each file
    checked <- sequence (sequence . (sequence . (sequence . (check <$>) <$>) <$>) <$> parsed)
    -- display results
    anyErrors <- sequence $ output <$> checked
    -- exit with appropriate status code
    if foldl' (||) False anyErrors
        then void exitFailure
        else void exitSuccess
