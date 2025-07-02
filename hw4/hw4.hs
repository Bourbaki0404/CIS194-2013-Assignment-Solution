greater_than_100 :: [Integer] -> [Integer]
greater_than_100 xs = filter (\n -> n > 100) xs 

fold :: b -> (a -> b -> b) -> [a] -> b
fold z _ []     = z
fold z f (x:xs) = f x (fold z f xs)

sum :: [Integer] -> Integer
sum = fold 0 (+)

my_length :: [t] -> Integer
my_length = fold 0 (\_ s -> s + 1)

my_foldl :: b -> (b -> a -> b) -> [a] -> b 
my_foldl z _ [] = z 
my_foldl z f (x:xs) = my_foldl (f z x) f xs

-- Wholemeal Programming
func1 :: [Integer] -> Integer
func1 = product . map (2-) . filter even

func2 :: Integer -> Integer
func2 = Main.sum . filter even . takeWhile (/= 1) . iterate next
    where 
        next n = if even n then n `div` 2 else 3 * n + 1

--  that each node stores an extra Integer representing the height at that node.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

-- If the tree can LL-rotate, then rotate it
rightRotation :: Tree a -> Tree a
rightRotation (Node _ (Node _ a x b) y c) = Node (1 + height a) a x (Node (1 + height c) b y c)
rightRotation t = t

balance :: Tree a -> Tree a
balance node@(Node _ (Node _ a _ _) _ c)
                        | height a > height c = rightRotation (node)
                        | otherwise           = node
balance t                                     = t


-- Insert a new node into the tree
-- Algorithm: always insert to the leftmost position of the tree, then use LL-Rotation to rebalancing
foldTreeHelper :: a -> Tree a -> Tree a
foldTreeHelper x Leaf = Node 0 Leaf x Leaf 
foldTreeHelper x (Node _ lhs y rhs) = balance (Node new_h new_lhs y rhs)
    where
        new_lhs = foldTreeHelper x lhs
        new_h   = 1 + max (height new_lhs) (height rhs)
        
foldTree :: [a] -> Tree a
foldTree xs = foldr foldTreeHelper Leaf xs

xor :: [Bool] -> Bool
xor xs = foldr (\x acc -> not acc) False xs

my_map :: (a -> b) -> [a] -> [b]
my_map f xs = foldr (\x acc -> (f x) : acc) [] xs

my_foldl2 :: (a -> b -> a) -> a -> [b] -> a
my_foldl2 f z xs = foldr (\x acc -> f acc x) z (reverse xs)

-- g: a -> a
my_foldl3 :: (a -> b -> a) -> a -> [b] -> a
my_foldl3 f z xs = foldr (\b g x -> g (f x b)) id xs z


cart_prod :: [a] -> [b] -> [(a, b)]
cart_prod xs ys = [(x, y) | x <- xs, y <- ys]

-- Given an integer n, your function should generate all the odd prime numbers up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : map (\x -> 2 * x + 1) included
    where
        excluded = map (\(i,j) -> i + j + 2 * i * j) (filter (\(i, j) -> i <= j && i + j + 2 * i * j <= n) (cart_prod [1..(n - 2) `div` 2] [1..(n - 2) `div` 2]))
        is_excluded m = foldr (\x acc -> x || acc) False (map (\x -> x == m) excluded)
        included = filter (not . is_excluded) [1..n]

