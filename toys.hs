--length' :: (Num b) => a -> b
--length' [] = 0
--length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci 3 = 2
fibonacci 4 = 3
fibonacci i = fibonacci (i-1) + fibonacci (i-2)

maximum' :: (Ord a) => [a] -> a
maximum' x = case x of [] -> error "maximux of empty list"
                       [x] -> x
                       (x:xs) -> max x (maximum xs)


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ a | a <- xs, a < x ]
        biggerSorted = quicksort [ a | a <- xs, a >= x ]
    in smallerSorted ++ [x] ++ biggerSorted

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
    where g x y = f y x


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
