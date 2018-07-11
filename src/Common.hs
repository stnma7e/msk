module Common
( breakChunks
, importantFrequencies
, slice
) where

breakChunks :: Int -> [a] -> [[a]]
breakChunks _ [] = []
breakChunks chunkSize y = let (x, xs) = splitAt chunkSize y
                in x : (breakChunks chunkSize xs)

importantFrequencies n gap =
    let x = [gap, 2 * gap .. gap * n]
    in zip x $ map (+gap) x

slice :: (Int, Int) -> [a] -> [a]
slice (start, stop) xs = take (stop - start + 1) $ drop start xs
