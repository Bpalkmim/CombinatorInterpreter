-- Bernardo Pinto de Alkmim
-- 1712658
-- Exercícios em Haskell para INF2811 - Programação Funcional
-- Professor Roberto Ierusalimschy
-- Parser genérico.
-- Comentários para o Parser iniciados com "--".
-- Caso se queira mudar isso, basta alterar a função "comment".

module Parser (
        Parser(Parser),
        parse,
        item,
        pfail,
        pnot,
        por,
        por1,
        sat,
        many,
        many1,
        many2,
        char,
        string,
        comment,
        space,
        spaces,
        token,
        symbol,
        digit,
        lower,
        upper,
        alpha,
        alphanum
) where

import Data.Char
import Control.Monad    (liftM)

data Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) = f

instance Functor Parser where
        fmap = liftM

instance Applicative Parser where
        pure x = Parser (\s-> [(x, s)])
        Parser f <*> Parser x = do
                g <- Parser f
                y <- Parser x
                pure (g y)

instance Monad Parser where
        return = pure
        x >>= f = Parser aux where
                aux s = concatMap aux' (parse x s)
                aux' (v, r) = parse (f v) r

item :: Parser Char
item = Parser temp where
        temp [] = []
        temp (x:xs) = [(x, xs)]

-- Falha ao parsear
pfail :: Parser a
pfail = Parser (\_-> [])

-- Negative lookahead
pnot :: Parser a -> Parser ()
pnot x = Parser (\s-> case parse x s of
        [] -> [((), s)]
        _ -> [])

-- Or não determinístico
por :: Parser a -> Parser a -> Parser a
por (Parser f) (Parser g) = Parser (\s-> f s ++ g s)

-- Or determinístico
por1 :: Parser a -> Parser a -> Parser a
por1 (Parser f) (Parser g) = Parser (\s-> case f s of
        [] -> g s
        x -> x)

-- Verifica se o caracter satisfaz a condição
sat :: (Char -> Bool) -> Parser Char
sat p = do
        c <- item
        if p c then
                return c
        else
                pfail

-- Fecho de Kleene ('*' de gramáticas)
many :: Parser a -> Parser [a]
many p = many1 p `por1` return []

-- '+' de gramáticas
many1 :: Parser a -> Parser [a]
many1 p = do
        x <- p
        xs <- many p
        return (x:xs)

-- Pelo menos 2 ocorrências (para aplicação)
many2 :: Parser a -> Parser [a]
many2 p = do
        x <- p
        xs <- many1 p
        return (x:xs)

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do
        char x
        string xs

comment :: Parser String
comment = do
        string "--"
        many (sat (/= '\n'))
        many space

space :: Parser Char
space = do
        c <- sat isSpace
        return c

spaces :: Parser String
spaces = do
        comment `por1` many space

-- Ignora espaços
token :: Parser a -> Parser a
token p = do
        x <- p
        spaces
        return x

symbol :: String -> Parser ()
symbol = token . string

digit :: Parser Char
digit = do
        c <- sat isDigit
        return c

lower :: Parser Char
lower = do
        c <- sat isLower
        return c

upper :: Parser Char
upper = do
        c <- sat isUpper
        return c

alpha :: Parser Char
alpha = do
        c <- lower `por1` upper
        return c

alphanum :: Parser Char
alphanum = do
        c <- alpha `por1` digit
        return c

main :: IO ()
main = do print "Parser"