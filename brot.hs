module Main where
import System.Environment
import System.Console.ANSI
import System.Exit
import Control.Monad
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
        w <- getArg args 0
        h <- getArg args 1
        let s = Size w h
        showBrot s $ mandelbrot s
        setSGR []

-- read a specific argument and check whether it's valid
-- if not, print message and exit
getArg :: [String] -> Int -> IO Int
getArg args i = case readMay $ args !! i of
                Nothing -> do 
                           putStrLn "You've entered an invalid argument"
                           exitFailure
                Just x  -> return x

-- display the given mandelbrot
showBrot :: Size -> Mandelbrot -> IO ()
showBrot _ [] = return ()
showBrot scr @ (Size w _) str = do 
    showLine line
    showBrot scr rest
    where (line, rest) = splitAt w str

-- display a single line of the mandelbrot
showLine :: [Int] -> IO ()
showLine []     = putStrLn ""
showLine (x:xs) = setColor x >> putChar (ascii x) >> showLine xs

-- given an intensity, sets a specific color
setColor :: Int -> IO ()
setColor i = setSGR [SetColor Foreground Vivid $ colors !! i] 
    where colors = [Black .. White] ++ [Red .. Yellow]

-- translate an intensity to a character
ascii :: Int -> Char
ascii i = chr $ ord ' ' + 10 - i

-- given a Size, create a mandelbrot
mandelbrot :: Size -> Mandelbrot
mandelbrot s @ (Size w h) = 
    [intensity x y s | y <- [0 .. h-1], x <- [0 .. w-1]]

-- compute intensity of the point at (x, y)
intensity :: Int -> Int -> Size -> Int
intensity x y (Size w h) = iteration 0 0 sx sy 0 10
    where 
    sx = -2.5 + 3.5 * fromIntegral x / fromIntegral w
    sy = -1 + 2 * fromIntegral y / fromIntegral h

-- helper function for intensity
iteration :: Double -> Double -> Double -> Double -> Int -> Int -> Int
iteration x y sx sy i mi 
    | x^2 + y^2 < 2*2 && i < mi = 
        iteration (x^2 - y^2 + sx) (2*x*y + sy) sx sy (i+1) mi
    | otherwise = i

