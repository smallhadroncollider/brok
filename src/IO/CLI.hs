{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.CLI where

import ClassyPrelude

import Data.Text.IO        (hPutStr, hPutStrLn)
import System.Console.ANSI (Color (Blue, Green, Red), ColorIntensity (Vivid),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor), hSetSGR)

message :: Text -> IO ()
message msg = do
    hSetSGR stderr [SetColor Foreground Vivid Blue]
    hPutStrLn stdout msg
    hSetSGR stderr [Reset]

errorMessage :: Text -> IO ()
errorMessage msg = do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    hPutStrLn stderr msg
    hSetSGR stderr [Reset]

errors :: Text -> [Text] -> IO ()
errors _ [] = return ()
errors msg missing = do
    errorMessage msg
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
