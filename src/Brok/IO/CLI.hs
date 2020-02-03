{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.CLI where

import ClassyPrelude

import Brok.Types.App      (App)
import Brok.Types.Config   (noColor)
import Data.Text.IO        (hPutStr, hPutStrLn)
import System.Console.ANSI (Color (Blue, Green, Magenta, Red, Yellow), ColorIntensity (Dull),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor), hClearLine,
                            hCursorUpLine, hSetSGR)

setSGR :: Handle -> [SGR] -> App ()
setSGR hndl settings = do
    colourize <- not <$> asks noColor
    when colourize $ lift (hSetSGR hndl settings)

blank :: App ()
blank = putStrLn ""

message :: Text -> App ()
message msg = do
    setSGR stdout [SetColor Foreground Dull Blue]
    putStrLn msg
    setSGR stdout [Reset]

mehssage :: Text -> App ()
mehssage msg = do
    setSGR stdout [SetColor Foreground Dull Yellow]
    putStrLn msg
    setSGR stdout [Reset]

header :: Text -> App ()
header msg = do
    setSGR stdout [SetColor Foreground Dull Magenta]
    putStrLn $ "*** " ++ msg ++ " ***"
    setSGR stdout [Reset]

successMessage :: Text -> App ()
successMessage msg = do
    setSGR stdout [SetColor Foreground Dull Green]
    putStrLn msg
    setSGR stdout [Reset]

errorMessage :: Text -> App ()
errorMessage msg = do
    setSGR stderr [SetColor Foreground Dull Red]
    lift $ hPutStrLn stderr msg
    setSGR stderr [Reset]

errors :: Text -> [Text] -> App ()
errors _ [] = return ()
errors msg missing = do
    errorMessage msg
    lift $ hPutStrLn stderr ""
    errorMessage (unlines $ ("- " ++) <$> missing)

split :: Handle -> Color -> Text -> Text -> App ()
split hdl color left right = do
    setSGR hdl [SetColor Foreground Dull color]
    lift $ hPutStr hdl left
    setSGR hdl [Reset]
    lift $ hPutStr hdl $ ": " ++ right
    lift $ hPutStrLn hdl ""

splitErr :: Text -> Text -> App ()
splitErr = split stderr Red

splitOut :: Text -> Text -> App ()
splitOut = split stdout Green

splitMeh :: Text -> Text -> App ()
splitMeh = split stdout Yellow

replace :: Text -> App ()
replace msg = do
    lift $ hCursorUpLine stdout 1
    lift $ hClearLine stdout
    putStrLn msg
