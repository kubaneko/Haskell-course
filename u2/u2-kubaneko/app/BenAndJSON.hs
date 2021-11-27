module BenAndJSON where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor            (bimap)
import           Data.ByteString.Lazy.UTF8
import qualified Data.Foldable             as F
import qualified Data.HashMap.Internal     as H
import           Data.Scientific
import           Data.Text
import qualified Data.Vector               as V

import           Parser

-- I was not able to get deriving Generic working so I had to write it on my own
-- makes JSON convertible to BenCode
instance FromJSON BenCode where
    parseJSON (Number n) = do getInt$toBoundedInteger n
                            where
                            getInt (Just i)=return$BenInt i
                            getInt _=       fail "not an integer"
    parseJSON (String s)= return $ BenStr$unpack s
    parseJSON (Array a) =do
                         ben <- mapM parseJSON (F.toList a)
                         return $ BenArr ben
    parseJSON (Object o) = BenDic <$> traverse parseTuple (H.toList o)
        where
        parseTuple (a, b) = do
          x <- parseJSON b
          return (unpack a, x)
    parseJSON j = fail ("not supported type" ++ show j)

-- makes BenCode convertible to JSON
instance ToJSON BenCode where
    toJSON (BenStr s)=String$ pack s
    toJSON (BenInt i)=Number$ fromIntegral i
    toJSON (BenArr a)=Array$V.fromList$ toJSON<$>a
    toJSON (BenDic d)=Object$ H.fromList$ bimap pack toJSON<$>d

{- TASK TWO -}

-- IO() -> IO() wrapper function takes json outputs bencode
json2bencode=do
            con <- getContents
            unwBen.eitherDecode$fromString con
            where
                unwBen (Left err)= print err
                unwBen (Right ben)=prettyPrint ben

-- IO() -> IO() wrapper function takes bencode outputs json
bencode2json=do
            con <- getContents
            printJsn$ parseBen con
            where
                printJsn (Left err)  = print err
                printJsn (Right ben) = putStrLn$"\n"++toString(encodePretty ben)
