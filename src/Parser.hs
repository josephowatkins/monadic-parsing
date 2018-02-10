-- Parser.hs

module Parser where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad
import Data.Char (isSpace, isDigit, ord)

newtype Parser a = Parser (String -> [(a, String)])

-- first parser: item
-- consume the first char of a string, fails on empty string
item :: Parser Char
item  = Parser (\cs -> case cs of
                         []     -> []
                         (c:cs) -> [(c, cs)])


-- before we can define a Monad need to define Functor and Applicative
instance Functor Parser where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f m = m >>= pure . f -- or just LiftM from Control.Monad

instance Applicative Parser where
  -- pure :: Applicative f => a -> f a
  pure a = Parser (\cs -> [(a,cs)])
  -- (<*>) ::  Applicative f => f (a -> b) -> f a -> f b
  f1 <*> f2 = f1 >>= \g -> f2 >>= (return . g)

-- define Monad instance for Parser
instance Monad Parser where
  -- return :: Monad m => a -> m a
  return = pure
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

parse :: Parser a -> String -> [(a,String)]
parse (Parser f) = f

-- example parser
p :: Parser (Char,Char)
p  = do {c <- item; item; d <- item; return (c,d)}

-- example applications:
-- parse p "abc" => [(('a','b'), "c")]
-- parse p "ab"  => []


-- choice combinators
-- to implement MonadPlus we also need to implement Alternative
instance Alternative Parser where
  -- (<|>) :: Alternative f => f a -> f a -> f a
  (<|>) = mplus
  -- empty :: Alternative f => f a
  empty = mzero

instance MonadPlus Parser where
  -- mzero :: m a
  mzero = Parser (\cs -> [])
  -- mplus :: MonadPlus m => m a -> m a -> m a
  p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)

-- define deterministic choice operator
(+++)  :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                     []     -> []
                     (x:xs) -> [x])

-- parser that takes a predicate:
sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

-- build parser that parses a specific char
char :: Char -> Parser Char
char c = sat (c ==)

-- recursion combinators:
-- parse a specific string
string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <- p
                    as <- many (do sep; p)
                    return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where rest a = (do f <- op
                                    b <- p
                                    rest (f a b))
                                +++ return a

-- lexical combinators
space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

-- example
expr  :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term   `chainl1` addop
term = factor `chainl1` mulop
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}
digit = do {x <- token (sat isDigit); return (ord x - ord '0')};

addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}
