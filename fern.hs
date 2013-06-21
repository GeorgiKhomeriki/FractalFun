module Main where
import System.Environment
import System.Random
import System.Console.ANSI
import System.Timeout
import System.IO
import Safe (readMay)

data Size   = Size Int Int
type Point  = (Float, Float) 

-- given width and height as arguments
-- prints an ascii Barnsley fern
main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "Usage: fern <width> <height>"
    else do
        let maybeSize = do
            w <- readMay $ head args
            h <- readMay $ args !! 1
            return $ Size w h
        maybeFern maybeSize

-- if arguments are valid, create and show fern
maybeFern :: Maybe Size -> IO ()
maybeFern Nothing  = putStrLn "You've provided an invalid argument"
maybeFern (Just s) = do
                hSetBuffering stdin NoBuffering
                clearScreen 
                fern s (0, 0)

-- calculates the next Point, scales and prints it
-- if 'q' isn't pressed repeats recursively
fern :: Size -> Point -> IO ()
fern s @ (Size w h) p = do
        (np, c) <- nextPoint p
        let (x, y) = scale s np
        setCursorPosition (h - round y) (round x)
        setSGR [SetColor Foreground Vivid c]
        putChar '^'
        input <- timeout 1 getChar
        case input of
            Just i | i == 'q'  -> setCursorPosition 0 0 >> setSGR [] >> clearScreen 
                   | otherwise -> step np
            Nothing            -> step np
    where step p = hFlush stdout >> fern s p

-- scale a point to screen coordinates
-- (−2.1820 < x < 2.6558, 0 ≤ y < 9.9983)
scale :: Size -> Point -> Point
scale (Size w h) (x, y) = (sx * fromIntegral w, sy * fromIntegral h)
    where
    sx = (x + 2.1820) / 4.8378
    sy = y / 9.9983

-- randomly selects the next tranformation
-- and returns the next Point with it's Color
nextPoint :: Point -> IO (Point, Color)
nextPoint p = do
                r <- randomRIO (1, 100) :: IO Int
                return $ transform r p

-- perform a tranformation given a random number
transform :: Int -> Point -> (Point, Color)                
transform r (x, y)
    | r == 1            = ((0, 0.16 * y)                                     , Yellow)
    | r >= 2  && r < 87 = ((0.85 * x + 0.04 * y, -0.04 * y + 0.85 * y + 1.6) , Red)
    | r >= 87 && r < 94 = ((0.2 * x - 0.26 * y, 0.23 * x + 0.22 * y + 1.6)   , Blue)
    | r >= 94           = ((-0.15 * x + 0.28 * y, 0.26 * x + 0.24 * y + 0.44), Green)

