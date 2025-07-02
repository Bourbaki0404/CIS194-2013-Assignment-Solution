{- CIS 194 HW 10
   due Monday, 1 April
-}


module AParser (Parser, runParser, satisfy, char, posInt) where
import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = Parser (fmap (\(a, str) -> (f a, str)) . runParser pa)

-- fmap f fa === pure f <*> fa 
instance Applicative Parser where
  pure a = Parser (\str -> Just (a, str))
  (Parser pra) <*> pa = Parser (\str -> case pra str of
                                          Nothing -> Nothing
                                          Just (f, s1) -> runParser (fmap f pa) $ s1)


abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\x _ z -> [x, z]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  (Parser a) <|> (Parser b) = Parser (liftA2 (<|>) a b)

intOrUppercase :: Parser ()
intOrUppercase = (pure (\_ -> ()) <*> posInt) <|> (pure (\_ -> ()) <*> satisfy isUpperCase)


a :: Int -> Maybe Int
a n = Just n

b :: Int -> Maybe Int
b n = Just n

-- Let's see why a -> is an applicative
f :: Int -> (Int -> Int)
f = pure (+1)

  







