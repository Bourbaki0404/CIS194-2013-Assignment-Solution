fib :: Integer -> Integer         
fib n
    | n > 1     = fib (n - 1) + fib (n - 2)
    | otherwise = 1

fibs1 :: [Integer]
fibs1 = map fib [0..]

my_repeat :: a -> [a]
my_repeat x = x : repeat x

my_take :: Integer -> [a] -> [a]
my_take _ []        = []
my_take n (x:xs)
        | n <= 0    = []
        | otherwise = x : my_take (n - 1) xs

fibs2 :: [Integer]
fibs2 = map fst (iterate (\ (a, b) -> (a + b, a)) (1, 0))

fibs3 :: [Integer]
fibs3 = fibs3Helper 0 1
    where
        fibs3Helper a b = a : (fibs3Helper b $ a + b) 

data Stream a = SC a (Stream a)

streamToList :: Stream a -> [a]
streamToList (SC x xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
    show s = show (take 20 $ streamToList s)

streamRepeat :: a -> Stream a
streamRepeat x = SC x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (SC x xs) = SC (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = SC x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (1+) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (SC x xs) ys = (SC x (interleaveStreams ys xs))

interleaveStreams2 :: Stream a -> Stream a -> Stream a
interleaveStreams2 (SC x xs) (SC y ys) = (SC x (SC y $ interleaveStreams xs ys))


ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (1+) ruler)

ruler2 :: Stream Integer
ruler2 = SC 0 xs 
    where
        (SC _ xs) = interleaveStreams2 (streamRepeat 0) (streamMap (1+) (SC 0 xs))

x :: Stream Integer
x = SC 0 (SC 1 (streamRepeat 0))

instance Num (Stream Integer) where
    (SC x xs) + (SC y ys) = SC (x + y) (xs + ys)
    xs - ys = xs + (negate ys)
    negate (SC x xs) = SC (negate x) (negate xs)
    fromInteger n = SC n $ streamRepeat 0
    (SC x xs) * ys@(SC y ys') = SC (x * y) (xs * ys + streamMap (x *) ys')

instance Fractional (Stream Integer) where
    (SC x xs) / (SC y ys) = q
      where
        q = SC (x `div` y) (streamMap (`div` y) (xs - q * ys))

fibs3' :: Stream Integer
fibs3' = x / (1 - x - x * x)

data Matrix a = Mat a a a a
    deriving (Show)

instance Num a => Num (Matrix a) where
    (Mat a b c d) * (Mat x y z m) = Mat (a*x + b*z) (a*y + b*m)
                                      (c*x + d*z) (c*y + d*m)
    -- You should also define (+), negate, fromInteger, abs, signum for a complete instance.

fibs4 :: Integer -> Integer
fibs4 n = j
    where
        (Mat _ j _ _) = (Mat 1 1 1 0) ^ n