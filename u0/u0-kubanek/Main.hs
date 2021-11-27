module Main where
import Data.Char (isAlpha)

flength :: [Char] -> Double
flength = fromIntegral . length

main :: IO ()
main = do
  str <- getContents
  if null $filter isAlpha str
    then putStrLn "No letters to count"
    else print $round $flength (filter (\a -> isAlpha a && elem a "aeiyou") str) /
         flength (filter isAlpha str) *
         100
