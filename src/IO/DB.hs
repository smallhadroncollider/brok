{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.DB
    ( load
    , cache
    ) where

import ClassyPrelude

import Data.Either           (fromRight)
import Data.Time.Clock       (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory      (doesFileExist)

import Parser.DB    (db)
import Parser.Links (Link)

path :: String
path = ".brokdb"

-- how long to cache links for
-- in seconds
age :: NominalDiffTime
age = 86400

removeOld :: [(Link, Integer)] -> IO [(Link, Integer)]
removeOld cached = do
    timestamp <- getPOSIXTime
    return $ filter (\(_, x) -> timestamp - fromInteger x < age) cached

-- write db
linkToText :: (Link, Integer) -> Text
linkToText (lnk, int) = concat [lnk, " ", tshow int]

write :: [(Link, Integer)] -> IO ()
write links = writeFile path . encodeUtf8 . unlines $ linkToText <$> links

stamp :: Link -> IO (Link, Integer)
stamp lnk = do
    timestamp <- round <$> getPOSIXTime
    return (lnk, timestamp)

cache :: [Link] -> IO ()
cache links = do
    current <- load
    stamped <- sequence (stamp <$> links)
    write $ current ++ stamped

-- read db
read :: FilePath -> IO [(Link, Integer)]
read filepath = removeOld =<< fromRight [] . db . decodeUtf8 <$> readFile filepath

load :: IO [(Link, Integer)]
load = do
    exists <- doesFileExist path
    if exists
        then read path
        else return []
