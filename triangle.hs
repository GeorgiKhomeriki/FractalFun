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
-- prints an ascii sierpinski triangle
main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "Usage: triangle <width> <height>"
    else do
        let maybeSize = do
            w <- readMay $ head args
            h <- readMay $ args !! 1
            return $ Size w h
        maybeTriangle maybeSize

-- if arguments are valid, create and show carpet
maybeTriangle :: Maybe Size -> IO ()
maybeTriangle Nothing  = putStrLn "You've provided an invalid argument"
maybeTriangle (Just s) = do
                hSetBuffering stdin NoBuffering
                clearScreen 
                triangle s (0, 0)

triangle :: Size -> Point -> IO ()
triangle s @ (Size w h) p = do
        (np, c) <- nextPoint p
        let (x, y) = scale s p
        setCursorPosition (h-(round y)) (round x)
        setSGR [SetColor Foreground Vivid c]
        putChar '^'
        input <- timeout 10 getChar
        case input of
            Just i | i == 'q'  -> setCursorPosition 0 0 >> setSGR [] >> clearScreen 
                   | otherwise -> step np
            Nothing            -> step np
    where step p = hFlush stdout >> triangle s p

scale :: Size -> Point -> Point
scale (Size w h) (x, y) = (x * fromIntegral w, y * fromIntegral h)

nextPoint :: Point -> IO (Point, Color)
nextPoint p @ (x, y) = do
                r <- randomRIO (0,2) :: IO Int
                return $ case r of
                    0 -> ((0.5 * x        , 0.5 * y)              , Yellow)
                    1 -> ((0.5 * x + 0.25 , 0.5 * y + sqrt 3 / 4) , Red)
                    2 -> ((0.5 * x + 0.5  , 0.5 * y)              , Blue)

