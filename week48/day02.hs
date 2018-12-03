module Main where

import Common

import Data.List
import Data.Maybe

work xs = c2 * c3

    where (c2, c3) =   length . filter fst
                   &&& length . filter snd
                   $ map one xs

          one xs   =   elem 2
                   &&& elem 3
                   $ length . (`elemIndices` xs) <$> xs

work' :: Eq a => [[a]] -> [[a]]
work' xs = catMaybes
         $ concat
         $ zipWith (map . f)
                   xs
                   (init $ drop 1 $ tails xs)

    -- This is run for all x out of the input list,
    -- and all y out of the elements of the input list that come after x.
    where f x y | g x y 0   = Just (mconcat
                                   -- Take in-common elements.
                                   $ zipWith (\u v -> if u == v
                                                         then pure u
                                                         else mempty)
                                             x y)
          f x y | otherwise = Nothing

          -- Check whether two strings differ by at most 1 character.
          g _  _  2 = False
          g [] [] n = n < 2
          g [] _  _ = False
          g _  [] _ = False
          g (x:xs) (y:ys) n | x == y    = g xs ys n
                            | otherwise = g xs ys (n+1)

p :: Parser Char [String]
p = some line <* end

main = withParsed p "day02.txt" (work &&& work')
