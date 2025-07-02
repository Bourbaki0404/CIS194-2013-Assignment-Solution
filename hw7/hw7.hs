import JoinList
import Sized
import Scrabble
import Buffer
import Editor
-- 
-- data Tree a = Empty | Node (Tree a) a (Tree a) 
-- 
-- treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
-- treeFold e _ Empty = e
-- treeFold e f (Node a x b) = f (treeFold e f a) x (treeFold e f b)
-- 
-- treeSize :: Tree a -> Integer
-- treeSize = treeFold (0::Integer) (\l _ r -> l + r + 1)
-- 
-- treeSum :: Tree Integer -> Integer
-- treeSum = treeFold (0::Integer) (\l x r -> l + x + r)
-- 
-- flatten :: Tree a -> [a]
-- flatten = treeFold [] (\l x r -> l ++ [x] ++ r)
-- 

-- Homework
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)


sizedToInt :: Sized m => m -> Int
sizedToInt = getSize . Sized.size

indexJ :: (Sized m, Monoid m) => 
            Int -> JoinList m b -> Maybe b
indexJ _ Empty = Nothing
indexJ i _  | i < 0 = Nothing
indexJ 0 (Single _ b) = Just b
indexJ _ (Single _ _) = Nothing
indexJ n (Append s xs ys)
    | n >= l_size       = indexJ (n - l_size) ys
    | otherwise         = indexJ n xs
    where l_size = sizedToInt $ tag xs 

dropJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i xs | i <= 0 = xs
dropJ _ (Single _ _) = Empty 
dropJ n (Append s xs ys)
    | n >= l_size = dropJ (n - l_size) ys
    | otherwise = Append (tag droppedLeft <> tag ys) droppedLeft ys 
    where 
        l_size = sizedToInt $ tag xs
        droppedLeft = dropJ n xs

takeJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ _ x@(Single _ _) = x
takeJ i (Append s xs ys)
    | i > l_size = Append (tag xs <> tag keptRight) xs keptRight
    | otherwise  = takeJ i xs
    where 
        l_size = sizedToInt $ tag xs 
        keptRight = takeJ (i - l_size) ys

jlSize :: Sized m => JoinList m a -> Int
jlSize Empty = 0
jlSize (Single _ _ ) = 1
jlSize (Append m _ _) = sizedToInt m

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList       
    fromString = foldl (+++) Empty . map (\s -> Single (scoreString s, Size 1) s). lines
    line = indexJ
    replaceLine n ln buf = takeJ n buf +++ fromString ln +++ dropJ (n + 1) buf
    numLines = jlSize 
    value = (\(Score n) -> n) . fst . tag

type JoinListBuffer = JoinList (Score, Size) String

jl :: JoinList Size Char
jl = Append (Size 3)
        (Single (Size 1) 'a')
        (Append (Size 2)
            (Single (Size 1) 'b')
            (Single (Size 1) 'c'))

testSize :: (Sized b, Monoid b) => JoinList b a -> Bool
testSize Empty = True 
testSize (Single s _) = sizedToInt s == 1
testSize (Append s xs ys) = testSize xs && testSize ys && sizedToInt s == left_size + right_size 
    where
        left_size  = sizedToInt . tag $ xs
        right_size = sizedToInt . tag $ ys 

jl10 :: JoinList Size Char
jl10 =
  Append (Size 10)
    (Append (Size 4)
      (Single (Size 1) 'a')
      (Append (Size 3)
        (Single (Size 1) 'b')
        (Append (Size 2)
          (Single (Size 1) 'c')
          (Single (Size 1) 'd'))))
    (Append (Size 6)
      (Append (Size 3)
        (Single (Size 1) 'e')
        (Append (Size 2)
          (Single (Size 1) 'f')
          (Single (Size 1) 'g')))
      (Append (Size 3)
        (Single (Size 1) 'h')
        (Append (Size 2)
          (Single (Size 1) 'i')
          (Single (Size 1) 'j'))))

-- main :: IO ()
-- main = do
--     let xs = jlToList jl10
--     mapM_ (\n -> print (n, let remain = dropJ n jl10 
--                             in jlToList remain == drop n xs && testSize remain)) [0..10]
--     mapM_ (\n -> print (n, indexJ n jl10 == (xs !!?n))) [0..10]
--     mapM_ (\n -> print (n, let remain = takeJ n jl10 
--                             in jlToList remain == take n xs && testSize remain)) [0..10]

main = runEditor editor xs 
    where
        xs :: JoinListBuffer 
        xs = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]


