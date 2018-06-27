module BreadthFirstTraversals where

import qualified Data.List as L

-- see https://doisinkidney.com/posts/2018-03-17-rose-trees-breadth-first.html


data Tree a = Node {
    root :: a
  , forest :: Forest a
}

type Forest a = [Tree a]

type Callback a = [Forest a] -> [a]

ex1 = [Node 1 [
        Node 2 [Node 5 [], Node 6 []],
        Node 3 [Node 7 [], Node 8 []],
        Node 4 []
      ]]

-- FIXME: rewrite with cont/zippers?
breadthFirst' :: Forest a -> [a]
breadthFirst' fs = (foldr f z0 fs) []

f :: Tree a -> ([Forest a] -> [a]) -> ([Forest a] -> [a])
f (Node x xs) cont = \fsts -> x : (cont (xs : fsts) )

z0:: [Forest a] -> [a]
z0 [] = []
z0 qs = (foldl (foldr f) z0 qs) []


-- ok, inefficient list appends ...

bf :: Forest a -> [a]
bf [] = []
bf (x:xs) =  f' x xs -- replace with fold?

-- bf' qs = foldr (\x r -> f' x) [] qs

f' :: Tree a -> Forest a -> [a]
f' (Node x rest) prevs = x : (bf (prevs ++ rest))

{-

-}

------------------------------------------------------
-- ok, more efficient list appends

bf' :: Forest a -> [a]
bf' (x:xs) = undefined

zf :: Tree a -> Forest a -> ([a] -> [a])
zf (Node x xs) conts = \prev -> x: (bf' $ conts ++ xs)

-- b' :: Forest a -> ([a] -> [a])
-- b' = undefined




-- foldr :: (a -> b -> b) -> b -> t a -> b
-- f :: Tree a -> ([Forest a] -> [a]) -> ([Forest a] -> [a])
-- (foldr f) :: ([Forest a] -> [a]) -> t (Tree a) -> [Forest a] -> [a]

{- execution∷


-}

-- FIXME: reimplement
breadthFirst :: Forest a -> [a]
breadthFirst ts = foldr f b ts []
  where
    f :: Tree a -> ([Forest a] -> [a]) -> ([Forest a] -> [a])
    f (Node x xs) fw = \prev -> x : fw (xs : prev)

    b :: [Forest a] -> [a]
    b [] = []
    b qs = foldl (foldr f) b qs []

levels :: Forest a -> [[a]]
levels = undefined
