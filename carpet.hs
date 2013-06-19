module Main where
import System.Environment
import Safe (readMay)

data Size   = Size Int Int
type Tile   = Bool
type Carpet = [Tile]

-- given width and height as arguments
-- prints an ascii sirpinski carpet
main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "Usage: carpet <width> <height>"
    else do
        let maybeSize = do
            w <- readMay $ head args
            h <- readMay $ args !! 1
            return $ Size w h
        maybeCarpet maybeSize

-- if arguments are valid, create and show carpet
maybeCarpet :: Maybe Size -> IO ()
maybeCarpet Nothing  = putStrLn "You've provided an invalid argument"
maybeCarpet (Just s) = showCarpet s $ carpet s

-- prints a carpet to the screen
showCarpet :: Size -> Carpet -> IO ()
showCarpet _ []             = return ()
showCarpet s @ (Size w _) c = do
                            mapM_ (putChar . showTile) line
                            putStrLn ""
                            showCarpet s rest
    where (line, rest) = splitAt w c

-- convert Tile to String
showTile :: Tile -> Char
showTile True  = '#'
showTile False = ' '

-- create a Carpet of given Size
carpet :: Size -> Carpet
carpet (Size w h) = [isInCarpet x y | y <- [1..h], x <- [1..w]]

-- check whether a point (x, y) is in the Carpet
isInCarpet :: Int -> Int -> Bool
isInCarpet x y
    | x > 0 || y > 0 = not ((x `mod` 3 == 1) && (y `mod` 3 == 1))
                       && isInCarpet (x `div` 3) (y `div` 3)
    | otherwise      = True
