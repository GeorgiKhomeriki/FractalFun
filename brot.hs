module Main where
import System.Environment
import System.Console.ANSI
import Control.Monad
import Data.Char

data Screen = Screen Int Int

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "Usage: brot width height"
    else do
        s <- return $ Screen (arg args 0) (arg args 1)
        showBrot s $ mandelbrot s
        setSGR []

arg :: Read a => [String] -> Int -> a
arg args i = read $ args !! i

showBrot :: Screen -> [Int] -> IO ()
showBrot _ [] = return ()
showBrot scr @ (Screen w _) str = do 
    showLine line
    showBrot scr $ drop w str
    where line = take w str

showLine :: [Int] -> IO ()
showLine []     = putStrLn ""
showLine (x:xs) = setColor x >> putChar (ascii x) >> showLine xs

setColor :: Int -> IO ()
setColor i = setSGR [SetColor Foreground Vivid $ colors !! i] 
    where colors = [Black .. White] ++ [Red .. Yellow]

ascii :: Int -> Char
ascii i = chr $ ord ' ' + (10-i)

mandelbrot :: Screen -> [Int]
mandelbrot s @ (Screen w h) = 
    [intensity x y s | y <- [0 .. h-1], x <- [0 .. w-1]]

intensity :: Int -> Int -> Screen -> Int
intensity x y s @ (Screen w h) = iteration 0 0 sx sy 0 10
    where 
    sx = -2.5 + 3.5 * fromIntegral(x) / fromIntegral(w)
    sy = -1 + 2 * fromIntegral(y) / fromIntegral(h)

iteration :: Double -> Double -> Double -> Double -> Int -> Int -> Int
iteration x y sx sy i mi 
    | x^2 + y^2 < 2*2 && i < mi = 
        iteration (x^2 - y^2 + sx) (2*x*y + sy) sx sy (i+1) mi
    | otherwise = i

