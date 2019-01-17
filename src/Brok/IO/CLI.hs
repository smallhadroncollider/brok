{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.CLI where

import ClassyPrelude

import Data.Text.IO        (hPutStr, hPutStrLn)
import System.Console.ANSI (Color (Blue, Green, Magenta, Red, Yellow), ColorIntensity (Dull),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor), hClearLine,
                            hCursorUpLine, hSetSGR)

message :: Text -> IO ()
message msg = do
    hSetSGR stdout [SetColor Foreground Dull Blue]
    hPutStrLn stdout msg
    hSetSGR stdout [Reset]

mehssage :: Text -> IO ()
mehssage msg = do
    hSetSGR stdout [SetColor Foreground Dull Yellow]
    hPutStrLn stdout msg
    hSetSGR stdout [Reset]

header :: Text -> IO ()
header msg = do
    hSetSGR stdout [SetColor Foreground Dull Magenta]
    hPutStrLn stdout $ "*** " ++ msg ++ " ***"
    hSetSGR stdout [Reset]

errorMessage :: Text -> IO ()
errorMessage msg = do
    hSetSGR stderr [SetColor Foreground Dull Red]
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
    hSetSGR hdl [SetColor Foreground Dull color]
    hPutStr hdl left
    hSetSGR hdl [Reset]
    hPutStr hdl $ ": " ++ right
    hPutStrLn hdl ""

splitErr :: Text -> Text -> IO ()
splitErr = split stderr Red

splitOut :: Text -> Text -> IO ()
splitOut = split stdout Green

splitMeh :: Text -> Text -> IO ()
splitMeh = split stdout Yellow

replace :: Text -> IO ()
replace msg = do
    hCursorUpLine stdout 1
    hClearLine stdout
    hPutStrLn stdout msg
