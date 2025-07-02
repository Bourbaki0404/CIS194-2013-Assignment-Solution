toDigit :: Integer -> [Integer]
toDigit x 
    | x < 10    = [x]
    | otherwise = (toDigit (x `div` 10))++[x `mod` 10]

toDigit2 :: Integer -> [Integer]
toDigit2 x = reverse (go x)
    where 
        go n
            | n < 10    = [n]
            | otherwise = n `mod` 10 : go (n `div` 10)

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper []           = []
doubleEveryOtherHelper (x:[])       = [x]
doubleEveryOtherHelper (x:(y:zs))   = (x:(2 * y:doubleEveryOtherHelper zs)) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleEveryOtherHelper (reverse l))

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + (sumList xs)

sumDigits :: [Integer] -> Integer
sumDigits []         = 0
sumDigits (x:xs)     = sumList (toDigit2 x) + (sumDigits xs) 


type Peg = String
type Move = (Peg, Peg)

-- number of disks, the source peg, the target peg, the aux peg -> a list of moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n x y z = reverse (hanoiHelper n x y z)
    where
        hanoiHelper n x y z
            | n > 0      = hanoi (n - 1) z y x ++ ((x, y) : hanoi (n - 1) x z y) 
            | otherwise  = []






