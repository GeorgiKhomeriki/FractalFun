module Main where
import System.Environment
import System.Console.ANSI
import Data.Char
import Safe (readMay)

data Size       = Size Int Int
type Mandelbrot = [Int]

-- given a width and height as parameters 
-- prints a colored ASCII mandelbrot 
main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "Usage: brot width height"
    else do
        let maybeSize = do
                    w <- readMay $ args !! 0
                    h <- readMay $ args !! 1
                    return $ Size w h
        maybeBrot maybeSize

-- checks whether Size is not Nothing
-- if so, creates and prints mandelbrot
maybeBrot :: Maybe Size -> IO ()
maybeBrot Nothing  = putStrLn "You've provided an invalid argument"
maybeBrot (Just s) = do
                    showBrot s $ mandelbrot s
                    setSGR []

-- display the given mandelbrot
showBrot :: Size -> Mandelbrot -> IO ()
showBrot _ []                = return ()
showBrot s @ (Size w _) brot = showLine line >> showBrot s rest
    where (line, rest) = splitAt w brot

-- display a single line of the mandelbrot
showLine :: [Int] -> IO ()
showLine = foldr showL $ putStrLn ""
    where showL x y = setColor x >> ascii x >> y

-- given an intensity, sets a specific color
setColor :: Int -> IO ()
setColor i = setSGR [SetColor Foreground Vivid $ colors !! i] 
    where colors = [Black .. White] ++ [Red .. Yellow]

-- print an intensity as a character
ascii :: Int -> IO ()
ascii i = putChar . chr $ ord ' ' + 10 - i

-- given a Size, create a mandelbrot
mandelbrot :: Size -> Mandelbrot
mandelbrot s @ (Size w h) = 
    [intensity x y s | y <- [0 .. h-1], x <- [0 .. w-1]]

-- compute intensity of the point at (x, y)
intensity :: Int -> Int -> Size -> Int
intensity x y (Size w h) = iteration 0 0 sx sy 0 10
    where 
    sx = -2.5 + 3.5 * fromIntegral x / fromIntegral w
    sy = -1.0 + 2.0 * fromIntegral y / fromIntegral h

-- helper function for intensity
iteration :: Double -> Double -> Double -> Double -> Int -> Int -> Int
iteration x y sx sy i mi 
    | x^2 + y^2 < 2*2 && i < mi = 
        iteration (x^2 - y^2 + sx) (2*x*y + sy) sx sy (i+1) mi
    | otherwise = i

