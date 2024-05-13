{-
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- MdToDoc
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module ParserMd
    ( parseMdFile
    , MdElement(..)
    , parseAndConvertMd
    , convertMdToDoc
    , parseLine
    ) where

import Parser(Parser(..), (<|>), parseChar, parseMany, parseSome)
import Control.Monad (void)
import Control.Applicative (optional)
--import Data.Maybe (fromMaybe)
--import Data.Char (isSpace)
import Types (Doc(..), defaultDoc)


data MdElement = MdHeader Int String
               | MdList [String]
               | MdParagraph String
               | MdText String
               deriving (Show, Eq)

parseMdFile :: String -> [Doc]
parseMdFile md = parseAndConvertMd md

parseAndConvertMd :: String -> [Doc]
parseAndConvertMd mdText = case runParser parseMdDocument mdText of
    Just (mdElements, _) -> convertMdToDoc mdElements
    Nothing -> []

parseMdDocument :: Parser [MdElement]
parseMdDocument = parseMany parseMdElement

parseMdElement :: Parser MdElement
parseMdElement = parseMdHeader <|> parseMdList <|> parseMdParagraph

parseMdHeader :: Parser MdElement
parseMdHeader = do
    hashes <- parseSome (parseChar '#')
    parseSome (parseChar ' ')
    text <- parseLine
    return $ MdHeader (length hashes) text

parseMdList :: Parser MdElement
parseMdList = do
    items <- parseMany parseListItem
    return $ MdList items

parseListItem :: Parser String
parseListItem = do
    parseChar '-'
    parseSome (parseChar ' ')
    parseLine

skipSpaces :: Parser ()
skipSpaces = void $ parseMany (parseChar ' ')

parseMdParagraph :: Parser MdElement
parseMdParagraph = do
    lines <- parseSome parseLine
    return $ MdParagraph (unlines lines)

parseLine :: Parser String
parseLine = parseSome (parseAnyCharExcept "\n") <* parseChar '\n'

parseAnyCharExcept :: String -> Parser Char
parseAnyCharExcept exclusions = satisfy (`notElem` exclusions)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \str -> case str of
    [] -> Nothing
    (x:xs) -> if f x then Just (x, xs) else Nothing

convertMdToDoc :: [MdElement] -> [Doc]
convertMdToDoc = map convertElement

convertElement :: MdElement -> Doc
convertElement (MdHeader level text) =
    defaultDoc { isTitle = True, string = text }
convertElement (MdList items) =
    defaultDoc { isList = True, list = map convertListItem items }
convertElement (MdParagraph text) =
    defaultDoc { isParagraph = True, string = text }

convertListItem :: String -> Doc
convertListItem item =
    defaultDoc { isString = True, string = item }