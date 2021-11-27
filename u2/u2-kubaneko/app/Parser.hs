{-# LANGUAGE FlexibleContexts #-}
-- not sure why this is needed :) - hint would be appreciated

module Parser where

import           Control.Monad
import           Data.Either
import           Data.Function
import           Data.Functor.Identity
import           Data.List
import           Data.Void
import           Text.Parsec
import           Text.Parsec.Char

-- Parser type used in this file
type BParsec a=Parsec String (State String (Maybe a)) a

-- BenCode data structure - not sure if String as only value is by the assignment
-- but it was on wiki
data BenCode=BenStr String | BenInt Int | BenArr [BenCode] | BenDic [(String, BenCode)]
                deriving (Eq)

-- sorts dictionary into correct order
sortBenDic=sortBy (compare `on` fst)

-- parser of BenCode - Not sure whether we should have parsed many1 here or if one is enough
pBen=pBenInt <|> pBenStr <|> pBenArr <|> pBenDic

{- DATA PARSERS -}
pBenInt=BenInt.(read) <$> between (char 'i') (char 'e') pInt

pStr=do
    long <- pUInt
    char ':'
    count (read long) anyChar

pBenStr=BenStr <$> pStr

pBenArr=BenArr<$>between (char 'l') (char 'e') (many pBen)

-- parses dictionary entry
pDicE=do
        key <- pStr
        val <- pBen
        return (key, val)

pBenDic=BenDic<$>do
        dic <- between (char 'd') (char 'e') (many pDicE)
        checkDic dic

checkDic res
        | res==sortBenDic res=return res
        | otherwise = parserFail "dictionary not ordered"

-- parses unsigned integers
pUInt=do
        num <- many1 digit
        verNum num 'x'

-- Helper function that validates BenCode standards on integers
verNum ['0'] '-'= parserFail "-0 not a number"
verNum ['0'] _=   return "0"
verNum [] _=      parserFail "not a number \"\""
verNum ('0':xs) _=parserFail$"not a correct number" ++ ('0':xs)
verNum n '-'=     return ('-':n)
verNum n _=       return n

-- parses ints
pInt=do
        neg <- option 'x' (char '-')
        num <- many1 digit
        verNum num neg

-- Function that takes BenCode and returns string
prettyBen :: BenCode -> [String]
prettyBen (BenStr i)=[show (length i) ++ ":" ++ i ++ "\n"]
prettyBen (BenInt i)=["i" ++ show i ++ "e\n"]
prettyBen (BenArr i)=["l\n"] ++ addSpace(concat(prettyBen<$> i)) ++ ["e\n"]
prettyBen (BenDic i)=["d\n"] ++ concat((\(a,b)-> addSpace(prettyBen(BenStr a)) ++ addNSpace 2 (prettyBen b))<$>sortBenDic i) ++ ["e\n"]

-- adds spaces to String in list of strings
addNSpace :: Int -> [String] -> [String]
addNSpace n b=(concat (replicate n " ") ++)<$>b
addSpace=addNSpace 1

-- parses Bencode on some input
parseBen=parse pBen ""

-- prettyPrints BenCode type
prettyPrint a=putStr ("\n" ++ concat(prettyBen a))

-- TaskOne - Reads BenCode from IO makes it pretty and spits it out
printIOBenPretty=do
        ben <-  parse pBen ""<$>getContents
        unwrap ben
        where
        unwrap (Left err)= print err
        unwrap (Right ben)=prettyPrint ben
