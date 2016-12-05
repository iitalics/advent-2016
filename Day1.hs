module Main where
import Data.List
import Data.List.Split(splitOn)

type Coord = (Int,Int)


main :: IO ()
main = splitOn ", " <$> getContents >>= print . runDirections

runDirections :: [String] -> Int
runDirections dirs = taxiDist s
  where
    taxiDist (x,y) = abs x + abs y
    (ds,s) = foldl' (flip f) ((1,0), (0,0)) dirs
    f (c:x) = applyDir c (read x)

applyDir c x (ds,s) = (ds', s')
  where
    ds' = turnFor c ds
    s' = s `cadd` (ds' `cmul` x)

turnFor 'L' = lturn
turnFor 'R' = rturn

cadd (x1,y1) (x2,y2) = (x1+x2, y1+y2)
cmul (x1,y2) a = (x1*a, y2*a)

lturn (x,y) = (-y,x)
rturn (x,y) = (y,-x)
