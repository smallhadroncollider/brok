{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok
    ( brok
    ) where

import ClassyPrelude

import System.Exit (exitFailure, exitSuccess)

import IO.CLI       (header, replace)
import IO.DB        (getCached, setCached)
import IO.Document  (readContent)
import IO.Http      (check)
import IO.Output    (output)
import Parser.Links (links)
import Types.Link   (getURL, isSuccess)
import Types.Result (cachedLinks, justLinks, linkIOMap, parseLinks, pathToResult)

-- entry point
brok :: IO ()
brok
    -- get files from command line
 = do
    files <- getArgs
    -- read files
    content <- sequence (readContent . pathToResult <$> files)
    -- find links in each file
    let parsed = parseLinks links <$> content
    -- check cached successes
    cached <- getCached
    let cache = cachedLinks cached <$> parsed
    -- check links in each file
    header "Checking URLs"
    putStrLn ""
    checked <- sequence (linkIOMap check <$> cache)
    replace "Fetching complete"
    -- display results
    putStrLn ""
    header "Results"
    anyErrors <- sequence $ output <$> checked
    -- cache successes
    setCached $ getURL <$> filter isSuccess (concat (justLinks <$> checked))
    -- exit with appropriate status code
    if foldl' (||) False anyErrors
        then void exitFailure
        else void exitSuccess
