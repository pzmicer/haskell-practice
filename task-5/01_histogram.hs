import System.Environment
import System.IO
import Data.Map


decompose :: String -> Map Char Int
decompose "" = empty
decompose s = go s empty
    where 
        go [] m = m
        go (c:cs) m = go cs (insertWith (+) c 1 m)
        

showHist :: Map Char Int -> String
showHist m = go (toList m) []
    where 
        go [] s = s
        go (x:xs) s = go xs (s ++ show (fst x) ++ ": " ++ show (snd x) ++ "\n")
    

printHist :: Map Char Int -> IO ()
printHist m = printHistHelper $ toList m

printHistHelper [] = return ()
printHistHelper (x:xs) = do
    putStrLn $ show (fst x) ++ ": " ++ show (snd x)
    printHistHelper xs


-- Pass filename as first argument
main :: IO ()
main = do
    filename <- fmap head getArgs
    fileContents <- readFile filename
    printHist $ decompose fileContents
