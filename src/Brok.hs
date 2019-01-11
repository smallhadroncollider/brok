{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok
    ( brok
    ) where

import ClassyPrelude

import System.Directory (doesFileExist)

import Parser.Links (links)

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
errors :: Text -> [Text] -> IO ()
errors _ []            = return ()
errors message missing = putStrLn message >> putStrLn (unlines missing)

parseArgs :: [Text] -> IO [Either TFilePath TFilePath]
parseArgs files = sequence (exists <$> files)

-- entry point
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
    putStrLn $ unlines (unlines <$> rights results)
