{-# LANGUAGE OverloadedStrings #-}

symbols =  ['*','-','+','$','=','#', '@', '!', '^', '&', '%', '~', '`', '?', '/', '+']
{- 
isBlank = (=='.') . snd 
parseRow = filter (not . isBlank) . (zip [0..])
flat (a,(b,c)) = ((a,b),c)
makeFlat xs = fmap flat $ concat $ fmap (\ (n, xs) -> zip (cycle [n]) xs ) $ zip [0..] xs 
 -}

addRow xs ys zs = zipWith3 add3 xs ys zs 

add3 "." "." "." = "."
add3 c "." "." = c 
add3 "." c "." = c
add3 "." "." c = c
add3 c1 c2 c3 = 'A'

main = do 
    x <- readFile "../data/3.txt"
    {-     -- let y =  fmap parseRow $ lines x -}    
    let rows = lines x
    print  $ zipWith3 (addRow) (rows) (drop 1 rows) (drop 2 rows) 