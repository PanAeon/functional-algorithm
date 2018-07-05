module LCS
    (lcsPlain,
     lcsSlow,
     lcsCached
    ) where

import           Data.List
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace

-- Write bloody test or whatever


-- FIXME: add bloody 0 to hoogle-authenticator!
-- FIXME: verify normally, I'm not sure below is correct, although looks so
lcsPlain :: Ord a => [a] -> [a] -> [a]
lcsPlain [] ys = []
lcsPlain xs [] = []
lcsPlain (x:xs) ys =  case dropWhile (/=x) ys of
                           []       -> lcsPlain xs ys
                           (_:ys'') -> max' (lcsPlain xs ys) (x: lcsPlain xs ys'')

max' xs ys = if length xs > length ys then xs else ys --hmm, length should be cached
    -- continuousSubsequenceLength xs ys = length $ takeWhile (uncurry (==)) $ zip xs ys
    -- subsequenceLength xs ys = undefined
      -- do
      --   tx <- tails xs
      --   ty <- tails ys

lcsSlow :: Ord a => [a] -> [a] -> [a]
lcsSlow [] ys = []
lcsSlow xs [] = []
lcsSlow (x:xs) (y:ys)
  | x == y    = x : lcsSlow xs ys
  | otherwise = max' (lcsSlow (x:xs) ys) (lcsSlow xs (y:ys))
--------------------------------------------------------------------------------


lcsCached :: Ord a => [a] -> [a] -> [a]
lcsCached xs ys = snd $ V.head $ if m > n then rys else rxs
  where
    n = length xs
    m = length ys
    xv = V.fromList xs
    yv = V.fromList ys
    (xs0, ys0) = (V.replicate (n+1) (0,[]), V.replicate (m+1) (0,[]))
    (rxs, rys) = foldl f (xs0, ys0) [1..min n m]
    f (row,col) i = (V.fromList $ row', V.fromList $  col')
      where
        xidx = n - i
        yidx = m - i
        xi = xv V.! xidx
        yi = yv V.! yidx

        row' = init $ scanr (fx xv yi row) (col V.! yidx) [0 .. xidx]
        col' = init $ scanr (fx yv xi col) (row V.! xidx) [0 .. yidx]

        fx ts t0 prevV l right =
          let
            bottomRight = prevV V.! (l + 1)
            bottom = prevV V.! l
          in if ts V.! l == t0
                 then (1 + fst bottomRight, t0 : (snd bottomRight))
                 else if fst right > fst bottom
                      then right
                      else bottom






-- good try, but no
-- foo xs ys = foldr f [] (tails xs)
--   where
--     max' xs ys = if length xs > length ys then xs else ys --FIXME: length should be cached, how?
--     f x prev = case dropWhile (/=x) ys of
--                 [] -> prev
--                 (_:ys'') -> max' prev
