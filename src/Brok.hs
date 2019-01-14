{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok
    ( brok
    ) where

import ClassyPrelude

import System.Directory (doesFileExist)
import System.Exit      (exitFailure, exitSuccess)

import CLI
import Http         (LinkStatus (..), check)
import Parser.Links (Link, ParseError, links)

type TFilePath = Text

-- IO
readIfTrue :: FilePath -> Bool -> IO (Maybe Text)
readIfTrue path True = Just <$> (decodeUtf8 <$> readFile path)
readIfTrue _ False   = return Nothing

readContent :: TFilePath -> IO (TFilePath, Maybe Text)
readContent path = do
    let filepath = unpack path
    status <- doesFileExist filepath >>= readIfTrue filepath
    return (path, status)

-- output
brokenOutput :: (Link, LinkStatus) -> IO ()
brokenOutput (url, Working)           = splitOut "  - OK" url
brokenOutput (url, Broken code)       = splitErr ("  -" ++ tshow code) url
brokenOutput (url, ConnectionFailure) = splitErr "  - Could not connect" url

statusError :: (Link, LinkStatus) -> Bool
statusError (_, Working) = False
statusError _            = True

countErrors :: [(Link, LinkStatus)] -> Int
countErrors statuses = length $ filter statusError statuses

outputPath :: TFilePath -> Text
outputPath path = concat ["\n", "[", path, "]"]

output :: (TFilePath, Maybe (Either ParseError [(Link, LinkStatus)])) -> IO Bool
output (path, Nothing) = do
    errorMessage $ outputPath path
    errorMessage "  - File Not found"
    return True
output (path, Just (Left err)) = do
    errorMessage $ outputPath path
    errorMessage "  - File could not be parsed"
    errorMessage err
    return True
output (path, Just (Right statuses)) = do
    let errs = countErrors statuses /= 0
    if errs
        then errorMessage $ outputPath path
        else message $ outputPath path
    sequence_ $ brokenOutput <$> statuses
    return errs

-- entry point
brok :: IO ()
brok = do
    files <- getArgs
    content <- sequence (readContent <$> files)
    let parsed = ((links <$>) <$>) <$> content
    checked <- sequence (sequence . (sequence . (sequence . (check <$>) <$>) <$>) <$> parsed)
    anyErrors <- sequence $ output <$> checked
    if foldl' (||) False anyErrors
        then void exitFailure
        else void exitSuccess
