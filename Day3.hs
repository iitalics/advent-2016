module Main where
import Data.List

main :: IO ()
main = (map parse . lines) <$> getContents >>= print . interp

parse :: String -> [Int]
parse = map read . words

interp = length . filter isValid


isValid :: [Int] -> Bool
isValid l | length l == 3 = a + b > c
  where
    [a,b,c] = sort l
isValid _ = False
