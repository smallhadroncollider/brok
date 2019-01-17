{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.Attoparsec where

import ClassyPrelude

import Data.Attoparsec.Text

lexeme :: Parser a -> Parser a
lexeme p = many' space *> p <* many' space

tchar :: Char -> Parser Text
tchar ch = singleton <$> char ch

chopt :: Char -> Parser Text
chopt ch = option "" (tchar ch)

concat3 :: (Monoid a) => a -> a -> a -> a
concat3 t1 t2 t3 = concat [t1, t2, t3]

concat4 :: (Monoid a) => a -> a -> a -> a -> a
concat4 t1 t2 t3 t4 = concat [t1, t2, t3, t4]

surround :: Char -> Char -> Parser Text -> Parser Text
surround open close parser = concat3 <$> tchar open <*> parser <*> tchar close
