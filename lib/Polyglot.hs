{-# LANGUAGE TupleSections #-}

-- A small parser combinator library.
-- Chris Smeele, 2018.

module Polyglot (module Polyglot
                ,(<|>)
                ,empty
                ,some
                ,many
                ,optional)
    where

import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Char
import           Data.List
import           Data.Maybe (listToMaybe)

newtype Parser a b = P ([a] -> [(b, [a])])

type SParser = Parser Char

parse' :: Parser a b -> [a] -> [(b, [a])]
parse' (P p) = p

parse :: Parser a b -> [a] -> Maybe b
parse p = listToMaybe . map fst . filter (null . snd) . parse' p

instance Semigroup s => Semigroup (Parser a s) where
    px <> py = do x <- px
                  y <- py
                  return $ x <> y
instance Monoid s => Monoid (Parser a s) where
    mempty = pure mempty

instance Functor (Parser a) where
    fmap g p = P (parse' p >>> fmap (\(x,rest) -> (g x, rest)))

instance Applicative (Parser a) where
    pure x    = P (pure . (x,))
    fg <*> fx = fg >>= (<$> fx)

instance Alternative (Parser a) where
    empty = P (const [])
    px <|> py = P (\xs -> parse' px xs ++ parse' py xs)

instance MonadPlus (Parser a)

instance Monad (Parser a) where
    p >>= f = P (concatMap (\(y,rest) -> parse' (f y) rest)
                . parse' p)

-- Take the intersection of two parsers.
(<&>) :: Eq b => Parser a b -> Parser a b -> Parser a b
pa <&> pb = P (\xs -> let ay = parse' pa xs
                          by = parse' pb xs
                       in intersectBy (\(ax,ar) (bx,br) ->
                                           ax == bx
                                        && (length ar == length br))
                                       ay
                                       by)

-- Feed the result of one parser into another.
($=>) :: Parser a [b] -> Parser b c -> Parser a c
px $=> py = P (parse' px
               >=> \(x, rest)
                -> case parse py x of
                     Just y  -> pure (y,rest)
                     Nothing -> empty)

item :: Parser a a
item = P item'
    where item' []     = empty
          item' (x:xs) = pure (x, xs)

sat :: (a -> Bool) -> Parser a a
sat p = item >>= \x -> if p x then pure x else empty

char :: Eq a => a -> Parser a a
char x = sat (== x)

string :: Eq a => [a] -> Parser a [a]
string = mapM char

-- Non-greedy variants of some and many.
some' p = (:) <$> p <*> many' p
many' p = pure []   <|> some' p

noneOf :: Eq a => [a] -> Parser a a
noneOf forbidden = sat (not . (`elem` forbidden))

epsilon :: Parser a ()
epsilon = pure ()

eol = void (char '\n')

eof = P f
    where f [] = pure ((),[])
          f _  = empty

end = many eol *> eof

digit = sat isDigit
alpha = sat isAlpha
alnum = sat isAlphaNum
space = sat isSpace
upper = sat isUpper
lower = sat isLower

posInteger :: Num a => Parser Char a
posInteger = foldl (\acc x -> acc * 10 + x) 0
           . map (fromIntegral . subtract (ord '0') . ord)
          <$> some digit

integer :: Num a => Parser Char a
integer =  (optional (char '+') *>             posInteger)
       <|> (          char '-'  *> (negate <$> posInteger))

line = many (noneOf "\n") <* eol

token p = many space *> p <* many space

string' = token . string

separated :: Parser a c -> Parser a b -> Parser a [b]
separated psep p = (:) <$> p <*> many (psep *> p)
