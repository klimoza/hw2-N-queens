module MyLib where

indToPair i n = ((i - 1) `div` n, (i - 1) `mod` n)

pairToInd a b n = a * n + b + 1

exactNum n = [[pairToInd i j n | j <- [0..(n - 1)]] | i <- [0..(n - 1)]]

generateRows n = [[- (pairToInd i j n), - (pairToInd i k n)] | i <- [0..(n - 1)], j <- [0..(n - 1)], k <- [0..(j - 1)]]

generateCols n = [[- (pairToInd j i n), - (pairToInd k i n)] | i <- [0..(n - 1)], j <- [0..(n - 1)], k <- [0..(j - 1)]]

generateMainDiags n = [[- (pairToInd i (i + d) n), - (pairToInd j (j + d) n)] | d <- [0..n], i <- [0..(n - 1 - d)], j <- [0..(i - 1)]] ++
                   [[- (pairToInd i (i - d) n), - (pairToInd j (j - d) n)] | d <- [0..n], i <- [d..(n - 1)], j <- [d..(i - 1)]] 

generatePrimDiags n = [[- (pairToInd i (d - i) n), - (pairToInd j (d - j) n)] | d <- [0..(2 * n)], i <- [(max 0 (d + 1 - n))..(min (n - 1) d)], j <- [(max 0 (d + 1 - n))..(i - 1)]]


generateFormula n = exactNum n ++ generateRows n ++ generateCols n ++ generateMainDiags n ++ generatePrimDiags n

parseSign :: (Ord a, Num a) => a -> [Char]
parseSign i | i < 0     = "."
            | otherwise = "Q"

printBoard :: (Integral a1, Num a2, Ord a2) => [a2] -> a1 -> [Char]
printBoard a n = helper 0 a n where
    helper _ [] _ = ""
    helper x (as : a) n 
        | x `mod` n == 0 && x /= 0 = "\n" ++ parseSign as ++ helper (x + 1) a n
        | otherwise                = parseSign as ++ helper (x + 1) a n
