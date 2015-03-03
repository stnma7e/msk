module Main
( main
) where

import Data.Complex

main = print . show $ fft . map (flip (:+) 0) $ [1..200]

fft :: [Complex Float] -> [Complex Float]
fft [] = []
fft input = let oddOrderedValues = odds input
                evenOrderedValues = evens input
                fftHalf1 = fft' oddOrderedValues evenOrderedValues (+) 0 (length input `div` 2) []
                fftHlaf2 = fft' oddOrderedValues evenOrderedValues (-) 0 (length input `div` 2) [] 
    in if (length oddOrderedValues) + (length evenOrderedValues) > 0
       then fftHalf1 ++ fftHlaf2
       else undefined
fft' :: [Complex Float] -> [Complex Float] -> (Complex Float -> Complex Float-> Complex Float) -> Int -> Int -> [Complex Float] -> [Complex Float]
fft' o e f k n list = let kth = (-2 * fromIntegral k * pi) / fromIntegral n
                          wk  = cis kth
                          yk  = (head e) `f` (wk * (head o))
    in if k < n
       then list ++ [yk] ++ fft' o e f (k + 1) n list
       else list

odds :: [a] -> [a]
odds [] = []
odds list = odds' [] list
odds' :: [a] -> [a] -> [a]
odds' list [] = list
odds' list (x:xs) = list ++ [x] ++ evens xs

evens :: [a] -> [a]
evens [] = []
evens list = odds . tail $ list
