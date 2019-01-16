{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.DB
    ( getCached
    , setCached
    ) where

import ClassyPrelude

import Data.Either           (fromRight)
import Data.Time.Clock       (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory      (doesFileExist)

import Parser.DB  (db)
import Types.Link (URL)

path :: String
path = ".brokdb"

-- how long to cache links for
-- in seconds
age :: NominalDiffTime
age = 86400

removeOld :: [(URL, Integer)] -> IO [(URL, Integer)]
removeOld cached = do
    timestamp <- getPOSIXTime
    return $ filter (\(_, x) -> timestamp - fromInteger x < age) cached

stamp :: URL -> IO (URL, Integer)
stamp lnk = do
    timestamp <- round <$> getPOSIXTime
    return (lnk, timestamp)

-- write db
linkToText :: (URL, Integer) -> Text
linkToText (lnk, int) = concat [lnk, " ", tshow int]

write :: [(URL, Integer)] -> IO ()
write links = writeFile path . encodeUtf8 . unlines $ linkToText <$> links

setCached :: [URL] -> IO ()
setCached links = do
    current <- load
    stamped <- sequence (stamp <$> links)
    write $ current ++ stamped

-- read db
read :: FilePath -> IO [(URL, Integer)]
read filepath = removeOld =<< fromRight [] . db . decodeUtf8 <$> readFile filepath

load :: IO [(URL, Integer)]
load = do
    exists <- doesFileExist path
    if exists
        then read path
        else return []

getCached :: IO [URL]
getCached = (fst <$>) <$> load
