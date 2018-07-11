module SongId
( chunkIdentifier
, findMaxFreq
, mapChunk2Freqs
, ft
) where

import Debug.Trace

import Data.Array.Unboxed (elems, assocs, UArray)
import Data.Array.IArray (Array(..), listArray, array)
import Data.Array.CArray (CArray)

import Data.Complex (Complex(..), magnitude)
import Data.Int (Int64, Int32, Int16, Int8)
import Math.FFT (dft)

import Data.List (sort)

import Common (importantFrequencies, breakChunks, slice)

chunkIdentifier :: [(Int, Int)] -> ([Float], [Complex Float]) -> [Float]
chunkIdentifier freqRanges (freqs, amps) =
    let breakFreqs chunk = map (flip slice chunk) freqRanges
        unzippedChunksInFrequencyRanges = map unzip $ breakFreqs $ zip freqs amps
        maxFreqsInRanges = map findMaxFreq $ unzippedChunksInFrequencyRanges
    in map fst maxFreqsInRanges

findMaxFreq :: ([Float], [Complex Float]) -> (Float, Complex Float)
findMaxFreq (xs, ys) = foldl (\max@(_, maxamp) new@(_, amp) ->
        if (magnitude amp) > (magnitude maxamp)
        then new
        else max)
    (0,0) $ zip xs ys

mapChunk2Freqs :: (Fractional a, Fractional b) => [a] -> ([b], [a])
mapChunk2Freqs chunk =
    let l = length chunk
        halfOfChunk = slice (0, l `quot` 2) chunk
        amps = map (/ fromIntegral l) halfOfChunk

        xs1 = map fromIntegral $ [0,1..l]
        xs2 = replicate l (44100 / fromIntegral l)
        freqs = map (\(x1,x2) -> x1 * x2) $ zip xs1 xs2
    in (freqs, amps)

ft :: Int -> UArray Int Int32 -> [CArray Int (Complex Float)]
ft chunkSize samples = map dft chunks
    where chunks = map (listArray (0, chunkSize - 1)) $ breakChunks chunkSize elements -- enumerate the samples in each chunk
          elements = map convertToComplex $ elems samples
          convertToComplex v = (fromIntegral v) :+ 0
