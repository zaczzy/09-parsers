{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Parsers where

import Control.Applicative
import Control.Monad (guard)
import Data.Char
import Text.Read (readMaybe)
import Prelude hiding (filter)

newtype Parser a = P {doParse :: String -> Maybe (a, String)}

get :: Parser Char
get = P $ \s -> case s of
  (c : cs) -> Just (c, cs)
  [] -> Nothing

oneDigit :: Parser Int
oneDigit = undefined

oneOp :: Parser (Int -> Int)
oneOp = P $ \s -> case s of
  ('-' : cs) -> Just (negate, cs)
  ('+' : cs) -> Just (id, cs)
  _ -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = undefined

--    SPOILER SPACE
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |
--     |

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = P $ \s -> do
  (c, cs) <- doParse get s
  guard (f c)
  return (c, cs)

filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> do
  (c, cs) <- doParse p s
  guard (f c)
  return (c, cs)

eof :: Parser ()
eof = P $ \s -> case s of
  [] -> Just ((), [])
  _ : _ -> Nothing

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = undefined

alphaChar, digitChar :: Parser Char
alphaChar = satisfy isAlpha
digitChar = satisfy isDigit

oneDigit' :: Parser Int
oneDigit' = cvt <$> digitChar -- fmap!
  where
    cvt :: Char -> Int
    cvt c = ord c - ord '0'

char :: Char -> Parser Char
char c = undefined

twoChar0 :: Parser (Char, Char)
twoChar0 = P $ \s -> do
  (c1, cs) <- doParse get s
  (c2, cs') <- doParse get cs
  return ((c1, c2), cs')

pairP0 :: Parser a -> Parser b -> Parser (a, b)
pairP0 = undefined

twoChar1 :: Parser (Char, Char)
twoChar1 = pairP0 get get

signedDigit0 :: Parser Int
signedDigit0 = P $ \s -> do
  (f, cs) <- doParse oneOp s
  (x, cs') <- doParse oneDigit cs
  return (f x, cs')

apP :: Parser (t -> a) -> Parser t -> Parser a
apP p1 p2 = P $ \s -> do
  (f, s') <- doParse p1 s
  (x, s'') <- doParse p2 s'
  return (f x, s'')

pureP :: a -> Parser a
pureP x = P $ \s -> Just (x, s)

instance Applicative Parser where
  pure = pureP
  (<*>) = apP

twoChar :: Parser (Char, Char)
twoChar = pure (,) <*> get <*> get

signedDigit :: Parser Int
signedDigit = oneOp <*> oneDigit

pairP :: Parser a -> Parser b -> Parser (a, b)
pairP p1 p2 = pure (,) <*> p1 <*> p2

pairP' :: Parser a -> Parser b -> Parser (a, b)
pairP' p1 p2 = (,) <$> p1 <*> p2

pairP'' :: Parser a -> Parser b -> Parser (a, b)
pairP'' = liftA2 (,)

tripleP :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
tripleP = liftA3 (,,)

-- | Parse something surrounded by parentheses
parenP :: Parser a -> Parser a
parenP p = char '(' *> p <* char ')'

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP = undefined

string :: String -> Parser String
string "" = pure ""
string (x : xs) = (:) <$> char x <*> string xs

string' :: String -> Parser String
string' = foldr undefined undefined

grabn :: Int -> Parser String
grabn n = if n <= 0 then pure "" else (:) <$> get <*> grabn (n -1)

chooseFirstP :: Parser a -> Parser a -> Parser a
p1 `chooseFirstP` p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing y = y

alphaNumChar :: Parser Char
alphaNumChar = alphaChar `chooseFirstP` digitChar

manyP :: Parser a -> Parser [a]
manyP p = ((:) <$> p <*> manyP p) `chooseFirstP` pure []

manyP' :: Parser a -> Parser [a]
manyP' p = pure [] `chooseFirstP` ((:) <$> p <*> manyP p)

failP :: Parser a
failP = P $ const Nothing

instance Alternative Parser where
  empty = failP -- always fail
  (<|>) = chooseFirstP -- try the left parser, if that fails then try the right

oneNat :: Parser Int
oneNat = fmap read (some digitChar) -- know that read will succeed because input is all digits

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = undefined

intOp :: Parser (Int -> Int -> Int)
intOp = plus <|> minus <|> times <|> divide
  where
    plus = char '+' *> pure (+)
    minus = char '-' *> pure (-)
    times = char '*' *> pure (*)
    divide = char '/' *> pure div

infixAp :: Applicative f => f a -> f (a -> b -> c) -> f b -> f c
infixAp = liftA3 (\i1 o i2 -> i1 `o` i2)

calc1 :: Parser Int
calc1 = infixAp oneNat intOp calc1 <|> oneNat

calcBad :: Parser Int
calcBad = infixAp calc1 intOp oneNat <|> oneNat

addOp :: Parser (Int -> Int -> Int)
addOp = char '+' *> pure (+) <|> char '-' *> pure (-)

mulOp :: Parser (Int -> Int -> Int)
mulOp = char '*' *> pure (*) <|> char '/' *> pure div

calc2 :: Parser Int
calc2 = addE

addE :: Parser Int
addE = infixAp mulE addOp addE <|> mulE

mulE :: Parser Int
mulE = infixAp factorE mulOp mulE <|> factorE

factorE :: Parser Int
factorE = oneNat <|> parenP calc2

type IntOp = Int -> Int -> Int

addE1 :: Parser Int
addE1 = process <$> first <*> rest
  where
    process :: Int -> [(IntOp, Int)] -> Int
    process = foldl comb

    comb :: Int -> (IntOp, Int) -> Int
    comb x (op, y) = x `op` y

    -- start with a multiplication expression
    first :: Parser Int
    first = mulE1

    -- parse any number of `addOp`s followed
    -- by a multiplication expression
    -- return the result in a list of tuples
    rest :: Parser [(IntOp, Int)]
    rest = many ((,) <$> addOp <*> mulE1)

mulE1 :: Parser Int
mulE1 = foldl comb <$> factorE1 <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> mulOp <*> factorE1)

factorE1 :: Parser Int
factorE1 = oneNat <|> parenP addE

chainl1 :: Parser Int -> Parser IntOp -> Parser Int
p `chainl1` pop = foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> pop <*> p)

addE2, mulE2, factorE2 :: Parser Int
addE2 = mulE2 `chainl1` addOp
mulE2 = factorE2 `chainl1` mulOp
factorE2 = parenP addE2 <|> oneNat
