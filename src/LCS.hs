module LCS
    (lcsPlain
    ) where

import           Data.List

-- Write bloody test or whatever

-- FIXME: configure stylish-haskell, use pomodoro, set fucking goals
-- FIXME: add bloody 0 to hoogle-authenticator!
lcsPlain :: Ord a => [a] -> [a] -> [a]
lcsPlain [] ys = []
lcsPlain xs [] = []
lcsPlain (x:xs) ys = let
                       ys' = dropWhile (/=x) ys
                      in case ys' of
                           []       -> lcsPlain xs ys
                           (_:ys'') -> x: lcsPlain xs ys''

    -- continuousSubsequenceLength xs ys = length $ takeWhile (uncurry (==)) $ zip xs ys
    -- subsequenceLength xs ys = undefined
      -- do
      --   tx <- tails xs
      --   ty <- tails ys
