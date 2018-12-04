module Main where

import           Common
import qualified Data.Set as S

work = sum

work' = f S.empty 0 . cycle
    where f acc n (x:xs) | n `S.member` acc = n
                         | otherwise        = f (S.insert n acc) (n+x) xs

p :: Parser Char [Int]
p = some (line $=> integer) <* end

main = withParsed p "day01.txt" (work &&& work')
