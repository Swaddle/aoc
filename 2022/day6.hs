import Text.Parsec
import Text.Parsec.String

import Control.Applicative 
import Data.Traversable (sequenceA)
import Data.List (tails, find, sort, nub)

transpose :: [[a]] -> [[a]]
transpose = getZipList . sequenceA . fmap ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose . take m . tails

part m = find ((==m) . length . nub . snd) . zip [m..]  . windows m  

--part1 = part 4 
--part2 = part 14

main = do 
    Right x <- parseFromFile (many1 letter) "data/6.txt"
    print $ part 4 x 
    print $ part 14 x 

