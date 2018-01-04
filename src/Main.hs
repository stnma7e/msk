module Main
( main
) where

import Codec.Wav (importFile)
import Data.Audio (Audio(..))
import Data.Array.Unboxed as AU (elems, UArray)
import Data.Complex (Complex(..))
import Data.Int (Int64)
import Numeric.Transform.Fourier.FFT (fft)
import Data.Array.IArray (Array(..), array)
import Control.Monad
import Control.Concurrent

type WordSize = Int64
chunkSize = 5000
files = ["05 I Will Follow You Into The Dark.wav", "06 Step Right Up.wav"]

main :: IO ()
main = foldl (\acc x -> acc >> x) (return ()) $ flip map files $ \filename -> do
    input <- importFile filename
    case input :: Either String (Audio WordSize) of
        Left err -> putStrLn err
        Right a@(Audio _ _ samples) -> do
            print a
            let fts = ft samples
            print $ length fts

ft :: UArray Int WordSize -> [Array Int (Complex Double)]
ft samples = map fft chunks
    where chunks = map (\x -> array (0, length x - 1) x) $ map (zip [0..chunkSize - 1]) elements
          elements = breakChunks $ map convertToComplex $ elems samples
          convertToComplex = (\v -> (fromIntegral v) :+ 0)

breakChunks :: [a] -> [[a]]
breakChunks [] = []
breakChunks y = let (x, xs) = splitAt chunkSize y
                in x : (breakChunks xs)
