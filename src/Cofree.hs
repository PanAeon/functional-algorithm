module Cofree
    (
    ) where

-- https://iokasimov.github.io/posts/2018/05/cofree-will-tear-us-apart

import           Control.Comonad.Cofree (Cofree (..))

type Stack = Cofree Maybe

stack :: Stack Int
stack = 1 :< Just (2 :< Just (3 :< Nothing))

-- FIXME: explore cofree, and comonad
