{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (liftA2 (:) p (zeroOrMore p)) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (liftA2 (:) p (zeroOrMore p)) <|> (liftA2 (:) p (pure []))

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)


------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = spaces *> (N <$> posInt <|> I <$> ident)

parse_left_p :: Parser Char
parse_left_p = spaces *> char '('

parse_right_p :: Parser Char
parse_right_p = spaces *> char ')'

parseSExpr :: Parser SExpr
parseSExpr = A <$> parseAtom <|> Comb <$> (parse_left_p *> zeroOrMore parseSExpr <* parse_right_p)


















































