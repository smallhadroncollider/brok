{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.CLI where

import ClassyPrelude

import Brok.Types.Config   (noColor)
import Brok.Types.Brok     (Brok, appConfig)
import Data.Text.IO        (hPutStr, hPutStrLn)
import System.Console.ANSI (Color (Blue, Green, Magenta, Red, Yellow), ColorIntensity (Dull),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor), hClearLine,
                            hCursorUpLine, hSetSGR)

setSGR :: Handle -> [SGR] -> Brok ()
setSGR hndl settings = do
    colourize <- not . noColor <$> asks appConfig
    when colourize $ lift (hSetSGR hndl settings)

blank :: Brok ()
blank = putStrLn ""

message :: Text -> Brok ()
message msg = do
    setSGR stdout [SetColor Foreground Dull Blue]
    putStrLn msg
    setSGR stdout [Reset]

mehssage :: Text -> Brok ()
mehssage msg = do
    setSGR stdout [SetColor Foreground Dull Yellow]
    putStrLn msg
    setSGR stdout [Reset]

header :: Text -> Brok ()
header msg = do
    setSGR stdout [SetColor Foreground Dull Magenta]
    putStrLn $ "*** " ++ msg ++ " ***"
    setSGR stdout [Reset]

successMessage :: Text -> Brok ()
successMessage msg = do
    setSGR stdout [SetColor Foreground Dull Green]
    putStrLn msg
    setSGR stdout [Reset]

errorMessage :: Text -> Brok ()
errorMessage msg = do
    setSGR stderr [SetColor Foreground Dull Red]
    lift $ hPutStrLn stderr msg
    setSGR stderr [Reset]

errors :: Text -> [Text] -> Brok ()
errors _ [] = return ()
errors msg missing = do
    errorMessage msg
    lift $ hPutStrLn stderr ""
    errorMessage (unlines $ ("- " ++) <$> missing)

split :: Handle -> Color -> Text -> Text -> Brok ()
split hdl color left right = do
    setSGR hdl [SetColor Foreground Dull color]
    lift $ hPutStr hdl left
    setSGR hdl [Reset]
    lift $ hPutStr hdl $ ": " ++ right
    lift $ hPutStrLn hdl ""

splitErr :: Text -> Text -> Brok ()
splitErr = split stderr Red

splitOut :: Text -> Text -> Brok ()
splitOut = split stdout Green

splitMeh :: Text -> Text -> Brok ()
splitMeh = split stdout Yellow

replace :: Text -> Brok ()
replace msg = do
    lift $ hCursorUpLine stdout 1
    lift $ hClearLine stdout
    putStrLn msg
