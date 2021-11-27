module Main where

--Task one
import           Parser     (printIOBenPretty)
--Task two
import           BenAndJSON (bencode2json, json2bencode)
--Task three
import           BenCheck   (bencode_check)

--To try them make them main
main :: IO ()
main=printIOBenPretty
--main=bencode2json
--main=json2bencode
--main=bencode_check
