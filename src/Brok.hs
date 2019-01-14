{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok
    ( brok
    ) where

import ClassyPrelude

import Data.Text.IO        (hPutStr, hPutStrLn)
import System.Console.ANSI (Color (Green, Red), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                            SGR (Reset, SetColor), hSetSGR)
import System.Directory    (doesFileExist)

import Http         (LinkStatus (..), check)
import Parser.Links (Link, links)

type TFilePath = Text

-- utility
boolToEither :: a -> Bool -> Either a a
boolToEither a True  = Right a
boolToEither a False = Left a

-- IO
exists :: TFilePath -> IO (Either TFilePath TFilePath)
exists file = boolToEither file <$> doesFileExist (unpack file)

readContent :: TFilePath -> IO (FilePath, Text)
readContent path = do
    let filepath = unpack path
    content <- decodeUtf8 <$> readFile filepath
    return (filepath, content)

-- CLI
errorMessage :: Text -> IO ()
errorMessage message = do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    hPutStrLn stderr message
    hSetSGR stderr [Reset]

errors :: Text -> [Text] -> IO ()
errors _ [] = return ()
errors message missing = do
    errorMessage message
    hPutStrLn stderr ""
    errorMessage (unlines $ ("- " ++) <$> missing)

split :: Handle -> Color -> Text -> Text -> IO ()
split hdl color left right = do
    hSetSGR hdl [SetColor Foreground Vivid color]
    hPutStr hdl left
    hSetSGR hdl [Reset]
    hPutStr hdl $ ": " ++ right
    hPutStrLn hdl ""

splitErr :: Text -> Text -> IO ()
splitErr = split stderr Red

splitOut :: Text -> Text -> IO ()
splitOut = split stdout Green

parseArgs :: [Text] -> IO [Either TFilePath TFilePath]
parseArgs files = sequence (exists <$> files)

-- entry point
brokenOutput :: (Link, LinkStatus) -> IO ()
brokenOutput (url, Working)           = splitOut "OK" url
brokenOutput (url, Broken code)       = splitErr (tshow code) url
brokenOutput (url, ConnectionFailure) = splitErr "Could not connect" url

brok :: IO ()
brok
    -- get files from command line
 = do
    files <- getArgs >>= parseArgs
    _ <- errors "Could not find files:" (lefts files)
    content <- sequence (readContent <$> rights files)
    -- parse links
    let results = uncurry links <$> content
    _ <- errors "Parsing errors:" (lefts results)
    -- check links
    checks <- sequence (check <$> rights results)
    -- output any broken links
    void . sequence $ brokenOutput <$> concat checks
