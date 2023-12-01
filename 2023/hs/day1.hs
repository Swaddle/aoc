import Data.Text as T (Text, splitOn, lines)
import Data.Text.IO as R (readFile)
import Data.Text.Read (decimal)

main = do 
    input <- R.readFile "../data/input.txt"
    putStrLn $ show $ input