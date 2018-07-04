module LCS
    (lcsPlain,
     lcsSlow,
     lcsCached
    ) where

import           Data.List
import           Data.Vector(Vector)
import   qualified Data.Vector as V
import Debug.Trace

-- Write bloody test or whatever

-- FIXME: use pomodoro, set fucking goals
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

-- FIXME: lcsCached "GXTXAB" "GGTAB"
-- lcsCached :: Ord a => [a] -> [a] -> [a]
lcsCached xs ys = if (fst . V.head . fst) res > (fst . V.head . snd ) res
                  then (snd . V.head . fst) res
                  else (snd . V.head . snd) res
  where
    n = length xs
    m = length ys
    n' = min n m
    -- xsr = reverse xs
    -- ysr = reverse ys
    xv = V.fromList xs
    yv = V.fromList ys
    (xs0, ys0) = (V.replicate (n+1) (0,[]), V.replicate (m+1) (0,[]))
    res = foldl f (xs0, ys0) [1..n']
    f (row,col) i = traceShowId $ (V.fromList $ row', V.fromList $  col')
      where
        xidx = n - i
        yidx = m - i
        xi = xv V.! xidx
        yi = yv V.! yidx
        -- FIXME: refactor this shit, and check this fucking reverse
        row' = tail $ reverse $  scanr fx (col V.! yidx) [0 .. xidx]

        col' = init $ scanr fy (row V.! xidx) [0 .. yidx]

        fx l right =
          let
            bottomRight = row V.! (l + 1)
            bottom = row V.! l
          in if xv V.! l == yi
                 then (1 + fst bottomRight, yi : (snd bottomRight))
                 else if fst right > fst bottom
                      then right
                      else bottom

        fy l bottom =
          let
            bottomRight = col V.! (l + 1)
            right = col V.! l
          in if yv V.! l == xi
                 then (1 + fst bottomRight, xi : (snd bottomRight))
                 else if fst bottom > fst right
                      then bottom
                      else right




-- good try, but no
-- foo xs ys = foldr f [] (tails xs)
--   where
--     max' xs ys = if length xs > length ys then xs else ys --FIXME: length should be cached, how?
--     f x prev = case dropWhile (/=x) ys of
--                 [] -> prev
--                 (_:ys'') -> max' prev
