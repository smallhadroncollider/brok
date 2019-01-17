{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.DB
    ( getCached
    , setCached
    ) where

import ClassyPrelude

import Data.Either           (fromRight)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory      (doesFileExist)

import Brok.Parser.DB  (db)
import Brok.Types.Link (URL)

path :: String
path = ".brokdb"

-- time stuff
removeOld :: Integer -> [(URL, Integer)] -> IO [(URL, Integer)]
removeOld age cached = do
    timestamp <- getPOSIXTime
    return $ filter ((\val -> timestamp - val < fromInteger age) . fromInteger . snd) cached

stamp :: URL -> IO (URL, Integer)
stamp lnk = do
    timestamp <- round <$> getPOSIXTime
    return (lnk, timestamp)

-- write db
linkToText :: (URL, Integer) -> Text
linkToText (lnk, int) = concat [lnk, " ", tshow int]

write :: [(URL, Integer)] -> IO ()
write links = writeFile path . encodeUtf8 . unlines $ linkToText <$> links

setCached :: Integer -> [URL] -> IO ()
setCached age links = do
    current <- load age
    stamped <- sequence (stamp <$> links)
    write $ current ++ stamped

-- read db
read :: Integer -> FilePath -> IO [(URL, Integer)]
read age filepath = removeOld age =<< fromRight [] . db . decodeUtf8 <$> readFile filepath

load :: Integer -> IO [(URL, Integer)]
load age = do
    exists <- doesFileExist path
    if exists
        then read age path
        else return []

getCached :: Integer -> IO [URL]
getCached age = (fst <$>) <$> load age
