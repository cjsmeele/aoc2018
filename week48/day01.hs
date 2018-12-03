module Main where

import Common

work = sum

work' = f [] 0 . cycle
    where f acc n (x:xs) | n `elem` acc = n
                         | otherwise    = f (n:acc) (n+x) xs

p :: Parser Char [Int]
p = some (line $=> integer) <* end

main = withParsed p "day01.txt" (work &&& work')
