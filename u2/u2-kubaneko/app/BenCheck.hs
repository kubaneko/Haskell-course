module BenCheck where

import           Data.Bool
import           Text.Parsec
import           Data.Map    (Map)
import qualified Data.Map    as Map

import           Parser

-- Assignment function
bencode_check=do
        ben <-  parse pBen ""<$>getContents
        unwrap ben
        where
        unwrap (Left err)= print err
        unwrap (Right ben)=sigErr$benCheck ben

-- helper unwrap IO function
sigErr False=putStrLn "\nchyba"
sigErr _=    putStrLn ""

-- Checks Arrays and additionally checks Dic Values which was not specified in assignment
benCheck (BenArr ben)
        | arrChecked ben=True
        | otherwise=False
benCheck (BenDic ((a,b):xs))=benCheck b && benCheck (BenDic xs)
benCheck _=True

-- Checks array for important things
arrChecked ben=uniformArrType ben && unifiedDicKey ben && unifiedAraAra ben

-- Checks if two BenCodes have same type

checkTypes (BenArr ben1) (BenArr ben2)=True
checkTypes (BenDic ben1) (BenDic ben2)=True
checkTypes (BenStr ben1) (BenStr ben2)=True
checkTypes (BenInt ben1) (BenInt ben2)=True
checkTypes _ _=                        False

-- Checks if type of array contents is uniform
uniformArrType []=    True
uniformArrType (x:xs)=uniformArr x xs
uniformArr _ []=     True
uniformArr x1 (x:xs)=checkTypes x1 x && uniformArr x1 xs

-- checks arr type uniformity - performed after - type uniformity check of arrays
unifiedAraAra (BenArr x1:BenArr x2:xs)=checkArrs x1 x2 && unifiedAraAra (BenArr x2:xs)
unifiedAraAra [BenArr x1]=arrChecked x1
unifiedAraAra _=True

-- Helper function for unifiedAraAra - checks unified arrays
checkArrs _ []=True
checkArrs [] _=True
checkArrs a b= arrChecked (a++b)


-- Checks uniformity of array Dictionaries - performed after - type uniformity check of arrays
unifiedDicKey (BenDic x1:BenDic x2: xs)=checkDics x1 x2 && unifiedDicKey (BenDic x2:xs)
unifiedDicKey [BenDic x1]=benCheck (BenDic x1)
unifiedDicKey _=True

-- Checks if two dictionaries for every key their entries are of the same type
-- takes their intersection with conflicts resolved by const
-- compares the right haskMap for subset with the intersection, where equality is equality of types
-- This is iff all conflicting keys have same type
-- Also Checks the entries
checkDics x1 x2=Map.isSubmapOfBy checkTypes (Map.intersectionWith const (Map.fromList x1) (Map.fromList x2)) (Map.fromList x2) && benCheck (BenDic x1) && benCheck (BenDic x2)

