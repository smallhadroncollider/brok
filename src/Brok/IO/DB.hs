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

import Brok.Parser.DB    (db)
import Brok.Types.Config (cache)
import Brok.Types.Brok   (Brok, appConfig)
import Brok.Types.URL    (URL)

path :: String
path = ".brokdb"

-- time stuff
removeOld :: Integer -> [(URL, Integer)] -> Brok [(URL, Integer)]
removeOld age cached = do
    timestamp <- lift getPOSIXTime
    return $ filter ((\val -> timestamp - val < fromInteger age) . fromInteger . snd) cached

stamp :: URL -> Brok (URL, Integer)
stamp lnk = do
    timestamp <- lift $ round <$> getPOSIXTime
    return (lnk, timestamp)

-- write db
linkToText :: (URL, Integer) -> Text
linkToText (lnk, int) = concat [lnk, " ", tshow int]

write :: [(URL, Integer)] -> Brok ()
write links = writeFile path . encodeUtf8 . unlines $ linkToText <$> links

setCached :: [URL] -> Brok ()
setCached links = do
    mAge <- cache <$> asks appConfig
    case mAge of
        Nothing -> pure ()
        Just age -> do
            current <- load age
            stamped <- traverse stamp links
            write $ current ++ stamped

-- read db
read :: Integer -> FilePath -> Brok [(URL, Integer)]
read age filepath = removeOld age =<< fromRight [] . db . decodeUtf8 <$> readFile filepath

load :: Integer -> Brok [(URL, Integer)]
load age = do
    exists <- lift $ doesFileExist path
    if exists
        then read age path
        else return []

getCached :: Brok [URL]
getCached = do
    mAge <- cache <$> asks appConfig
    case mAge of
        Nothing    -> pure []
        (Just age) -> (fst <$>) <$> load age
