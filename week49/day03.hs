module Main where

import           Common
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed         as V
import           Control.Monad.ST
import           Control.Monad

width  = 1000
height = 1000

vindex x y = y * width + x

data Claim = C { cId   :: Int
               , cPos  :: (Int,Int)
               , cSize :: (Int,Int)
               }
               deriving (Show)

-- Generate a list of vector indices for the given claim.
claimIndices :: Claim -> [Int]
claimIndices (C _ (x,y) (w,h))
  = [vindex x' y' | y' <- [y..y+h-1]
                  , x' <- [x..x+w-1]]

-- Generate a 1000x1000 vector counting the claimants per cell.
toVec :: [Claim] -> V.Vector Int
toVec claims
   = runST $ do vec <- M.replicate (width*height) 0
                forM_ claims (poke vec)
                V.freeze vec
    where poke v c
            = mapM_ (M.modify v (+1)) (claimIndices c)

-- Count the squares that have more than one claimant.
work :: [Claim] -> Int
work = V.length . V.filter (>1) . toVec

-- Narrow down to claims of which the entire area has only one claimant.
work' :: [Claim] -> [Claim]
work' claims  = filter f claims
    where f c = all (==1) $ (toVec claims V.!) <$> claimIndices c

p :: Parser Char [Claim]
p = some (C <$> (char '#' *> integer)
            <*> ((,) <$> (string " @ " *> integer) <*> (char ',' *> integer))
            <*> ((,) <$> (string ": "  *> integer) <*> (char 'x' *> integer))
            <* eol)
         <* end

main = withParsed p "day03.txt" (work &&& work')
