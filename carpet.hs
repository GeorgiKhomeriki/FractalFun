module Main where
import System.Environment
import System.Console.ANSI
import Safe (readMay)

data Size   = Size Int Int
type Tile   = Bool
type Carpet = [Tile]

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "Usage: carpet <width> <height>"
    else do
        let maybeSize = do
            w <- readMay $ args !! 0
            h <- readMay $ args !! 1
            return $ Size w h
        maybeCarpet maybeSize

maybeCarpet :: Maybe Size -> IO ()
maybeCarpet Nothing  = putStrLn "You've provided an invalid argument"
maybeCarpet (Just s) = showCarpet s $ carpet s

showCarpet :: Size -> Carpet -> IO ()
showCarpet _ []             = return ()
showCarpet s @ (Size w _) c = do
                            mapM (putStr . showTile) line
                            putStrLn ""
                            showCarpet s rest
    where (line, rest) = splitAt w c

showTile :: Tile -> String
showTile True  = "#"
showTile False = " "

carpet :: Size -> Carpet
carpet (Size w h) = [isInCarpet x y | y <- [1..h], x <- [1..w]]

isInCarpet :: Int -> Int -> Bool
isInCarpet x y
    | x > 0 || y > 0 = if (x `mod` 3 == 1) && (y `mod` 3 == 1)
                       then False
                       else isInCarpet (x `div` 3) (y `div` 3)
    | otherwise      = True
