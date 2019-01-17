{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Brok
    ( brok
    ) where

import ClassyPrelude

import Data.FileEmbed (embedFile)
import System.Exit    (exitFailure, exitSuccess)

import           Brok.IO.CLI       (header, replace)
import           Brok.IO.DB        (getCached, setCached)
import           Brok.IO.Document  (readContent)
import           Brok.IO.Http      (check)
import           Brok.IO.Output    (output)
import           Brok.Options      (parse)
import           Brok.Parser.Links (links)
import qualified Brok.Types.Config as C (Config, cache, files, interval)
import           Brok.Types.Link   (getURL, isSuccess)
import           Brok.Types.Result (cachedLinks, justLinks, linkIOMap, parseLinks, pathToResult)

go :: C.Config -> IO ()
go config
    -- read files
 = do
    content <- sequence (readContent . pathToResult <$> C.files config)
    -- find links in each file
    let parsed = parseLinks links <$> content
    -- check cached successes
    cached <- getCached (C.cache config)
    let cache = cachedLinks cached <$> parsed
    -- check links in each file
    header "Checking URLs"
    putStrLn ""
    checked <- sequence (linkIOMap (check (C.interval config)) <$> cache)
    replace "Fetching complete"
    -- display results
    putStrLn ""
    header "Results"
    anyErrors <- sequence $ output <$> checked
    -- cache successes
    setCached (C.cache config) $ getURL <$> filter isSuccess (concat (justLinks <$> checked))
    -- exit with appropriate status code
    if foldl' (||) False anyErrors
        then void exitFailure
        else void exitSuccess

-- entry point
brok :: IO ()
brok = do
    config <- parse <$> getArgs
    case config of
        Right cnf -> go cnf
        Left _ -> do
            putStrLn "Invalid format"
            putStr $ decodeUtf8 $(embedFile "test/data/links.md")
            void exitFailure
