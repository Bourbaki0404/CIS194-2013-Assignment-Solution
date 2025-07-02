data List t = E | C t (List t)
    deriving Show
list1 :: List Integer
list1 = C 3 (C 5 (C 7 E))

mapList :: (t -> m) -> List t -> List m
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
        | p x = C x (filterList p xs)
        | otherwise  = filterList p xs

findFirstEven :: [Integer] -> Maybe Integer
findFirstEven [] = Nothing
findFirstEven (x : xs)
            | x `mod` 2 == 0 = Just x
            | otherwise      = findFirstEven xs

-- input, n, pos
skip_n :: [t] -> Integer -> [t]
skip_n xs n = skip_n_helper xs n 1
    where
        skip_n_helper [] _ _         = []
        skip_n_helper (x:xs) n i
                    | i `mod` n == 0 = x : skip_n_helper xs n (i + 1) 
                    | otherwise      = skip_n_helper xs n (i + 1) 

n_list :: Integer -> [Integer]
n_list n = reverse (n_list_rev n)
    where
        n_list_rev n
                | n > 0     = n : n_list_rev (n - 1)
                | otherwise = []

-- my_length :: [t] -> Integer
-- my_length = foldr (\_ acc -> acc + 1) 0

skips :: [t] -> [[t]]
skips xs = map (\n -> skip_n xs n) (n_list (my_length xs))

-- return the local maxima of the list
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = [] 
localMaxima (x:[y]) = []
localMaxima (x:y:z:xs)
        | x > y && z > y = y : localMaxima (y:(z:xs))
        | otherwise      = localMaxima (y:(z:xs))
               
-- A list of number, k, the number of k. All number should be between 0 and 9
count_k :: [Integer] -> Integer -> Integer
count_k [] _        = 0
count_k (x:xs) k
        | x == k    = 1 + count_k xs k
        | otherwise = count_k xs k

histogram_helper :: [Integer] -> [Integer]
histogram_helper xs = map (\n -> count_k xs n) [0..9]

histogram_line_k :: [Integer] -> Integer -> Integer -> String
histogram_line_k xs k n = map (\x -> if ((n - k) + 1 <= x) then '*' else ' ') xs 

histogram :: [Integer] -> String
histogram xs = all_lines ++ footer
    where
        num_count = histogram_helper xs
        max_count = maximum num_count
        all_lines = unlines (map (\n -> histogram_line_k num_count n max_count) [1..max_count])
        footer = "==========\n0123456789\n"