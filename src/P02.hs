{-# LANGUAGE LambdaCase #-}
module P02() where

import Data.List

-----------------------------------------------
-- maximum surpaser
-- looks odd, but probably correct, at least to O(n log n)
maxSurpasser:: Ord a => [a] -> Int
maxSurpasser xs = maximum (ys)
  where
    xs' = (\case {(j, (i, v)) -> (j,i,v)}) <$> (zip [0..] $ sortOn snd (zip [0..] xs))
    l   = length xs'
    xs'' = groupBy g xs'
    g (_, _, v0) (_, _, v1) = v0 == v1
    ys = h <$> xs''
    h xs = let
            (j, i, _) = head xs
           in  l - j -i - (length xs)

msc :: Ord a => [a] -> Int
msc xs = maximum [scount z zs | z:zs <- tails' xs]

scount z zs = length $ filter (>z) zs

tails' [] = []
tails' (x:xs) = (x:xs) : tails xs
