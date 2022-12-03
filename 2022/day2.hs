import Text.Parsec
import Text.Parsec.String

{-# 
    A Rock 0
    B Paper 3 
    C Scissors 6   
#-}

parsePair :: Parser (Int, Int)
parsePair = do
  first <- choice [ 0 <$ char 'A', 3 <$ char 'B', 6 <$ char 'C' ]
  space
  second <- choice [ 0 <$ char 'X', 3 <$ char 'Y', 6 <$ char 'Z' ]
  pure (first, second)

parseStrategy :: Parser [(Int, Int)]
parseStrategy = parsePair `sepEndBy` newline

score1 (a,b) = (b+3-a) `mod` 9 + (b `div` 3) + 1
part1 = sum . fmap (score1) 

score2 (a,b) = (((b+a-3) `mod` 9) `div` 3) + b + 1 
part2 = sum . fmap (score2) 

main = do 
    Right x <- parseFromFile parseStrategy "data/2.txt"  
    print $ part1 x
    print $ part2 x  
