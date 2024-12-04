import Data.Char (isDigit, isAlpha)

rot xs = take 2 (drop ((length xs)-1) (cycle xs))
count :: String -> Integer
count = read . reverse . rot . (filter isDigit) 
part1 = sum . fmap (count) . lines

main = do 
    x <- readFile "../data/1.txt"
    print $ part1 x 
