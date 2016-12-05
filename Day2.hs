module Main where
import Data.List

main :: IO ()
main = lines <$> getContents >>= print . concat . map show . keypad 5

up x | x < 4 = x
     | otherwise = x - 3

down x | x > 6 = x
       | otherwise = x + 3

left x | x `mod` 3 == 1 = x
       | otherwise = x - 1

right x | x `mod` 3 == 0 = x
        | otherwise = x + 1

op 'U' = up
op 'D' = down
op 'L' = left
op 'R' = right

keypad :: Int -> [String] -> [Int]
keypad x [] = []
keypad x (line:rest) = x':keypad x' rest
  where
    x' = foldl' (flip op) x line

