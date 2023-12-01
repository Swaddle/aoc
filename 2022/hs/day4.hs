import Text.Parsec
import Text.Parsec.String
import Data.List 

parseLine :: Parser (Int,Int,Int,Int)
parseLine = do 
    x <- many1 digit 
    char '-'
    y <- many1 digit 
    char ','
    z <- many1 digit 
    char '-'
    t <- many1 digit 
    pure (read x ,read y ,read z ,read t)

parseAll :: Parser [(Int,Int,Int,Int)]
parseAll = parseLine `sepEndBy` newline

contains (a,b,c,d) = a <= c && b >= d || c <= a && d >= b
overlaps (a,b,c,d) = a <= d && b >= c 

part1 = length . filter contains 
part2 = length . filter overlaps

main = do 
    Right x <- parseFromFile parseAll "data/4.txt"  
    print $ part1 x 
    print $ part2 x 