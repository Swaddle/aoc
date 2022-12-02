{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T (Text, splitOn, lines,head,last)
import Data.Text.IO as R (readFile)
import Data.Text.Read (decimal)
import Data.Either (rights)
import Data.List
import Data.Ord



main = do 
    x <- R.readFile "data/2.txt"
    let y = T.lines x
    print $ (\ x -> (T.head x, T.last x)) <$> y