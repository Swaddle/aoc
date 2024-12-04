import Text.Parsec
import Text.Parsec.String
import Data.List 
import Data.Char 
import Data.Set as S (fromList, toList)

parseLines :: Parser [String] 
parseLines = many (many letter <* endOfLine)

chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)
priority = flip mod 58 . (14 +) . abs . (52 -) . ord

rmDuplicates = S.toList . S.fromList
part1 = sum . fmap (sum . fmap priority . rmDuplicates . foldr1 intersect . (chunks . (`div`2) =<< length))
part2 = sum . fmap (sum . fmap priority . rmDuplicates . foldr1 intersect) . chunks 3 

main = do 
    Right x <- parseFromFile parseLines "data/3.txt"  
    print $ part1 x 
    print $ part2 x
