
import Text.Parsec
import Text.Parsec.String
import Control.Monad (forM_)
import Control.Monad.State.Lazy as S 
import Data.List (transpose) 
import Data.Maybe 

type Move = (Int,Int,Int)

crateP :: Parser Char
crateP = do
    char '['
    x <- upper
    char ']'
    pure x 

maybeCrateP:: Parser (Maybe Char)
maybeCrateP = (try (Nothing <$ string "   ")) <|> fmap Just crateP

lineP :: Parser [Maybe Char]
lineP = maybeCrateP `sepBy1` (char ' ') 

maybeCratesP :: Parser [[Maybe Char]]
maybeCratesP = lineP `sepEndBy1` endOfLine 

cratesP :: Parser [[Char]]
cratesP =  fmap catMaybes . transpose <$> maybeCratesP

integerP :: Parser Int 
integerP = do
    x <- many1 digit
    pure $ read x 

moveP :: Parser Move
moveP = (,,)   
    <$> (string "move " *> integerP)
    <*> (string " from " *> integerP) 
    <*> (string " to " *> integerP)

movesP = moveP `sepEndBy1` endOfLine

idP :: Parser Int
idP = do
    (char ' ')
    x <- integerP 
    (char ' ')
    pure x

idsP :: Parser [Int]
idsP = do
    x <- idP `sepBy1` (char ' ')
    endOfLine 
    endOfLine
    pure x

inputP:: Parser ([[Char]],[Move])
inputP = do
    crates<-cratesP
    ids <- idsP
    moves<- movesP
    pure (crates,moves)

getCrates :: Int -> S.State [[a]] [a]
getCrates  = state . go
    where
        go i (xs : xss) = case i of
            1 -> let (xs', xs'') = (head xs, tail xs) in (xs', xs'' : xss)
            _ -> let (xs', xss') = go (i - 1) xss in (xs', xs : xss')

getCratesN :: Int -> Int -> S.State [[a]] [a]
getCratesN amount  = state . go
    where
        go i (xs : xss) = case i of
            1 -> let (xs', xs'') = splitAt amount xs in (xs', xs'' : xss)
            _ -> let (xs', xss') = go (i - 1) xss in (xs', xs : xss')

putCrates :: Int -> [a] -> S.State [[a]] ()
putCrates i xs' = modify $ go i
    where
        go i (xs : xss) = case i of
            1 -> (xs' ++ xs) : xss
            _ -> xs : go (i - 1) xss

stepBy1 :: Move -> S.State [[a]] ()
stepBy1 (amount, from, to) = replicateM_ amount $ getCrates from >>= putCrates to

stepChunkN :: Move -> S.State [[a]] ()
stepChunkN (amount, from, to) = getCratesN amount from >>= putCrates to

part1 (crates,moves) = head <$> execState (mapM_ stepBy1 moves) crates
part2 (crates,moves) = head <$> execState (mapM_ stepChunkN moves) crates

main = do 
    Right x <- parseFromFile inputP "data/5.txt"
    print $ part1 x
    print $ part2 x 