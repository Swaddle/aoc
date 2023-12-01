{-# LANGUAGE OverloadedStrings #-}
import Data.Text as T (Text, splitOn, lines)
import Data.Text.IO as R (readFile)
import Data.Text.Read (decimal)
import Data.Either (rights)
import Data.List
import Data.Ord

parse x =  (fmap fst) <$> rights <$> (fmap decimal ) <$> (T.lines) <$> splitOn "\n\n" x

part1 = maximum . fmap sum . parse 
part2 = sum . take 3 . sortBy (comparing Down) . fmap sum . parse 

main = do 
    x <- R.readFile "../data/1.txt"
    print (part1 x)
    print (part2 x)