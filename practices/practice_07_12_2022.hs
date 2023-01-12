import System.Environment
import System.IO


readLinewise :: Handle -> Int -> IO ()
readLinewise file count = do
    isEOF <- hIsEOF file
    if isEOF then return ()
    else do
        line <- hGetLine file
        putStrLn $ show count ++ ": " ++ line
        readLinewise file (count+1)

main :: IO ()
main = do
    filename <- fmap head getArgs
    file <- openFile filename ReadMode
    readLinewise file 0
    hClose file

-- Alternative form
-- main = getArgs >>= \a ->
--     openFile (head a) ReadMode >>= \f ->
--     hGetLine f >>= \l ->
--     putStrLn l >>
--     hClose f