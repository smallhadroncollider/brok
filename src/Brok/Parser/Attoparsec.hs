{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.Attoparsec where

import ClassyPrelude

import Data.Attoparsec.Text

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

tchar :: Char -> Parser Text
tchar ch = singleton <$> char ch

chopt :: Char -> Parser Text
chopt ch = option "" (tchar ch)

manyChars :: Parser Char -> Parser Text
manyChars p = pack <$> many1 p

concat3 :: (Monoid a) => a -> a -> a -> a
concat3 t1 t2 t3 = concat [t1, t2, t3]

concat5 :: (Monoid a) => a -> a -> a -> a -> a -> a
concat5 t1 t2 t3 t4 t5 = concat [t1, t2, t3, t4, t5]

surround :: Char -> Char -> Parser Text -> Parser Text
surround open close parser = concat3 <$> tchar open <*> parser <*> tchar close
