import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char 
import Data.Char 
import Control.Arrow ((***))
import Data.List 
import Control.Monad.State

main = do 
    Right y <- parseFromFile pCommands "data/7.txt"
    let (FSZipper fsitem xs) = fsRoot $ foldl' runFS (FSZipper (Folder "/" []) []) y
    print $ part1 fsitem 
    print $ part2 fsitem

-- Attempt 2
-- Zippers from learn yourself haskell 

type Name = String 
type Size = Int 
data FSItem = File Size Name | Folder Name [FSItem] deriving (Show)
data FSNode = FSNode Name [FSItem] [FSItem] deriving (Show)
data FSZipper = FSZipper FSItem [FSNode] 
data Command = Root | Up | Down Name | LS [FSItem] deriving (Show) 

--foldl' f z [] = z
--foldl' f z (x:xs) = let z' = z `f` x  in seq z' $ foldl' f z' xs

pName :: Parser Name
pName = many1 $ satisfy (\ c-> isAlpha c || c== '.')

pSize :: Parser Size
pSize = read <$> many1 (digit) 

pRoot :: Parser Command
pRoot = Root <$ string "$ cd /" <* endOfLine

pUp :: Parser Command
pUp = Up <$ string "$ cd .." <* endOfLine

pDown :: Parser Command
pDown = Down <$> (string "$ cd " *> pName) <* endOfLine

pList :: Parser Command
pList = LS <$ string "$ ls" <* endOfLine <*> many (try pDir <|> try pFile)
  
pDir :: Parser FSItem
pDir = Folder <$> (string "dir " *> pName) <*> pure [] <* endOfLine

pFile :: Parser FSItem
pFile = File <$> pSize <*> (char ' ' *> pName) <* endOfLine

pCommands :: Parser [Command]
pCommands = many1 $ try pRoot <|> try pUp <|> try pDown <|> try pList


fsUp :: FSZipper -> FSZipper
fsUp (FSZipper (item) (FSNode name ls rs:bs)) = FSZipper (Folder name (ls ++ [item] ++ rs)) bs

fsRoot :: FSZipper -> FSZipper
fsRoot (FSZipper (item) ([])) = FSZipper (item) ([])
fsRoot x = fsRoot . fsUp $ x 

nameOf :: FSItem -> Name
nameOf (File _ name) = name
nameOf (Folder name _) = name

runFS :: FSZipper -> Command -> FSZipper
runFS t Root = fsRoot t
runFS t Up = fsUp t
runFS t (Down name) = FSZipper (dir) (FSNode parent ls rs : bs)
    where
        FSZipper (Folder parent items) bs = t
        (ls, dir : rs) = break ((== name) . nameOf) $ items
runFS t (LS items) = FSZipper (Folder name items) bs
    where
        FSZipper (Folder name _) (bs) = t

part :: FSItem -> (Int, Int)
part (File size _) = (0, size)
part (Folder _ items) 
    | b <= 100000 = (a + b, b)
    | otherwise =  (a, b)
    where
        (a, b) = foldr ((\(x, y) -> (+ x) *** (+ y)) . part) ( 0, 0) items

part1 :: FSItem -> Int
part1 = fst . part

part2' :: FSItem -> ([Int], Int)
part2' (File size _) = ([], size)
part2' (Folder _ items) = (b : a, b)
  where
    (a, b) = foldr ((\(a, b) -> (++ a) *** (+ b)) . part2') ([], 0) items

part2 :: FSItem -> Int
part2 fs = head . filter ((>= 30000000) . (70000000 -) . (head xs -)) . sort $ xs
    where 
        (xs,_) = part2' fs 