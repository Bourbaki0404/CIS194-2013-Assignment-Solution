import Employee
import Data.Tree 


main :: IO ()
main = putStrLn "hello world" >> putStrLn "Fuck you"

main1 :: IO ()
main1 = putStrLn "Please input a integer" >> (readLn >>= \n -> putStrLn (show (n + 1)))


instance Semigroup GuestList where
    (GL al af) <> (GL bl bf) = GL (al++bl) (af+bf)

instance Monoid GuestList where
    mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es tf) = GL (e : es) (tf + (empFun e))

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL xs f1) gl2@(GL ys f2)
    | f1 > f2   = gl1
    | otherwise = gl2
   

treeFold :: ([b] -> a -> b) -> b -> Tree a -> b
treeFold f z (Node {rootLabel = x, subForest = as}) = f (map (treeFold f z) as) $ x

-- (with boss, without boss) -> boss
nextLevel :: [(GuestList, GuestList)] -> Employee -> (GuestList, GuestList)
nextLevel xs boss = (withBoss, withoutBoss)
    where
        withBoss = glCons boss $ (foldl (<>) mempty $ map snd xs)
        withoutBoss = foldl (<>) mempty $ map fst xs

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel (mempty, mempty)

glFormat :: GuestList -> String
glFormat (GL l fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
    where
        employees = map empName l

main_2 :: IO ()
main_2 = readFile "company.txt" >>= putStrLn . glFormat . maxFun . read