module Main
( main
) where

import Debug.Trace

import Codec.Wav (importFile)
import Data.Audio (Audio(..))

import Data.Array.Unboxed (elems, assocs)
import Data.Array.IArray (Array(..), listArray, array)
import Data.Complex (magnitude)
import Data.Int (Int64, Int32, Int16, Int8)

import Common (importantFrequencies, breakChunks, slice)
import SongId (ft, mapChunk2Freqs, chunkIdentifier)

type WordSize = Int32
chunkSize = 5000
file = ""

main :: IO ()
main = do
    input <- importFile file
    song_dft <- case input :: Either String (Audio WordSize) of
        Left err -> do
            putStrLn err
            return [(array (0,0) [])]
        Right a@(Audio _ _ samples) -> return (ft chunkSize samples)

    putStrLn $ show (length song_dft) ++ " " ++ show (length . elems $ head song_dft)

    let ys = map (mapChunk2Freqs . elems) song_dft

    let freqs = importantFrequencies 5 (8 * chunkSize `quot` 50 `quot` 2)
    let chunks = map (chunkIdentifier freqs) ys
    foldl (\acc x -> acc >> print x) (return ()) $ chunks
