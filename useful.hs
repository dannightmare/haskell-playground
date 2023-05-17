
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ a | a <- xs, a < x ]
        biggerSorted = quicksort [ a | a <- xs, a >= x ]
    in smallerSorted ++ [x] ++ biggerSorted
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | odd n = n:collatz (n*3 + 1)
    

--collatz_me :: (Num a) => a -> [(a, a)]
collatz_me x = zip [ length arr | arr <- map collatz [1..x] ] [1..x]
-- can run 
-- maximum (collatz_me x)
-- to find the maximum length of the collatz sequence
numLongChains :: Int  
numLongChains = length (filter isLong (map collatz [1..100]))  
    where isLong xs = length xs > 15 
