module LCS
    (lcsPlain
    ) where

import           Data.List

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
  where
    max' xs ys = if length xs > length ys then xs else ys --hmm, length should be cached
    -- continuousSubsequenceLength xs ys = length $ takeWhile (uncurry (==)) $ zip xs ys
    -- subsequenceLength xs ys = undefined
      -- do
      --   tx <- tails xs
      --   ty <- tails ys

--------------------------------------------------------------------------------

-- good try, but no
-- foo xs ys = foldr f [] (tails xs)
--   where
--     max' xs ys = if length xs > length ys then xs else ys --FIXME: length should be cached, how?
--     f x prev = case dropWhile (/=x) ys of
--                 [] -> prev
--                 (_:ys'') -> max' prev
