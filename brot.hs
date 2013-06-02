module Main where
import System.Environment
import System.Console.ANSI
import Data.Char

data Screen = Screen Int Int

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "Usage: brot width height"
    else do
        clearScreen
        setCursorPosition 0 0
        s <- return $ Screen (arg args 0) (arg args 1)
        showBrot s $ createBrot s

arg :: Read a => [String] -> Int -> a
arg args i = read $ args !! i

showBrot :: Screen -> String -> IO ()
showBrot _ [] = return ()
showBrot scr @ (Screen w _) str = do 
    putStrLn $ take w str
    showBrot scr $ drop w str

createBrot :: Screen -> String
createBrot s @ (Screen w h) = 
    [ascii $ intensity x y s | x <- [0..w-1], y <- [0..h-1]]

ascii :: Int -> Char
ascii i = chr $ ord ' ' + (i-1) `div` 100

intensity :: Int -> Int -> Screen -> Int
intensity x y s @ (Screen w h) = iteration 0 0 sx sy 0 1000 
    where 
    sx = -2.5 + 3.5 * fromIntegral(x) / fromIntegral(w)
    sy = -1 + 2 * fromIntegral(y) / fromIntegral(h)

iteration :: Float -> Float -> Float -> Float -> Int -> Int -> Int
iteration x y sx sy i mi 
    | x*x + y*y < 2*2 && i < mi = 
        iteration (x*x - y*y + sx) (2*x*y + sy) sx sy (i+1) mi
    | otherwise = i