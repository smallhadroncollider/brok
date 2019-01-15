{-# LANGUAGE NoImplicitPrelude #-}

module Brok
    ( brok
    ) where

import ClassyPrelude

import System.Exit (exitFailure, exitSuccess)

import Http         (LinkStatus (..), broken)
import IO.DB        (cache, load)
import IO.Document  (TFilePath, readContent)
import IO.Output    (output)
import Parser.Links (Link, links)

joinEither :: Either a (Either a b) -> Either a b
joinEither (Left a)          = Left a
joinEither (Right (Left a))  = Left a
joinEither (Right (Right b)) = Right b

data LinkCheck
    = Cached Link
    | NeedsChecking Link
    deriving (Show)

cacheCheck :: [(Link, Integer)] -> Link -> LinkCheck
cacheCheck cache lnk =
    case find ((== lnk) . fst) cache of
        Just _  -> Cached lnk
        Nothing -> NeedsChecking lnk

cached :: (TFilePath, Either Text [Link]) -> IO (TFilePath, Either Text [LinkCheck])
cached lnks = do
    cache <- load
    return $ ((cacheCheck cache <$>) <$>) <$> lnks

broken' :: LinkCheck -> IO (Link, LinkStatus)
broken' (Cached lnk)        = return (lnk, Working True)
broken' (NeedsChecking lnk) = broken lnk

check' :: [LinkCheck] -> IO [(Link, LinkStatus)]
check' lnks = sequence (broken' <$> lnks)

success :: (Link, LinkStatus) -> Bool
success (_, Working False) = True
success _                  = False

cacheSuccesses :: [(TFilePath, Either Text [(Link, LinkStatus)])] -> IO ()
cacheSuccesses files = do
    let lnks = concat $ rights (snd <$> files)
    let successes = fst <$> filter success lnks
    cache successes

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
    -- check cached successes
    cache <- sequence (cached <$> parsed)
    -- check links in each file
    putStrLn "*** Checking URLs ***"
    checked <- sequence (sequence . (sequence . (check' <$>) <$>) <$> cache)
    -- display results
    putStrLn "\n*** Results ***"
    anyErrors <- sequence $ output <$> checked
    -- cache successes
    cacheSuccesses checked
    -- exit with appropriate status code
    if foldl' (||) False anyErrors
        then void exitFailure
        else void exitSuccess
