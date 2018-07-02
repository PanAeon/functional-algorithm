module P03() where

import           Debug.Trace
-- improving on saddleback search

-- f :: (Int,Int) => Int
f1 :: (Int,Int) -> Int
f1 (x,y) = x^2 + y^3

-- (z+1)^2 evaluation of
invert1 :: ( (Int, Int) -> Int) -> Int -> [(Int, Int)]
invert1 f z = [ (x,y) | x <- [0..z], y <- [0..z], f(x,y) == z]

-- f(x,y) >= x + y
invert2 f z = [ (x,y) | x <- [0..z], y <- [0..z-x], f(x,y) == z]

invert2' f z =  [ (x,y) | x <- [0..z'], y <- [0..z'-x], f(x,y) == z]
  where
    z' = z - f (0,0)

--- now saddleback search

find1 :: (Int, Int) -> ( (Int, Int) -> Int) -> Int -> [(Int, Int)]
find1 (u,v) f z = [(x,y) | x <- [u..z], y <- [v,v-1..0], f (x,y) == z]

invert1' f z = find1 (0,z) f z

invert3 f z = find3 (0,z) f z
find3 (u,v) f z
  | u > z || v < 0 = []
  | z' < z = find3 (u+1, v) f z
  | z' == z = (u,v): find3 (u+1, v-1) f z
  | z' >  z = find3 (u, v-1) f z
    where
      z' = f (u,v)

-- m' f z = maximum $ filter (\y -> f(0,y) <= z) [0..z]
-- n' f z = maximum $ filter (\x -> f(x,0) <= z) [0..z]

-- ok, sorted, could be done by binary search:
bsearch g l r z
       | l + 1 == r = l
       | g m <= z = bsearch g m r z
       | otherwise = bsearch g l m z
         where
           m = (l + r) `div` 2

m'' ::( (Int, Int) -> Int) -> Int -> Int
m'' f z = bsearch (curry f 0) 0 z z
n'' ::  ( (Int, Int) -> Int) -> Int -> Int
n'' f z = bsearch ((flip . curry) f 0) 0 z z

invert f z = find (0,m) f z
  where
    m = bsearch (curry f 0) 0 z z
find (u,v) f z
  | u > n || v < 0 = []
  | z' < z = find3 (u+1, v) f z
  | z' == z = (u,v): find3 (u+1, v-1) f z
  | z' >  z = find3 (u, v-1) f z
    where
      z' = f (u,v)
      n = bsearch ((flip . curry) f 0) 0 z z

-- complexity is 2logz + m + n

-- TODO: it's no good simply copying this alg, although it is simple,
-- and I've done it before. I need to reimplement it myself.
-- same goes for below alg. in principle this is alg. construction
-- exercise, it's easy to impl. them without any proof

-- =================================================================
-- now bsearch
-- FIXME: do bsearch && saddleback on your own later

-- find' :: (Int, Int) -> (Int, Int) -> ((Int, Int) -> Int) -> Int -> [(Int, Int)]
find' (u,v) (r,s) f z
  | u > r || v < s = []
  | v - s <= r - u = rfind (bsearch (\x -> f (x,q)) (u-1) (r+1) z)
  | otherwise      = cfind ( bsearch (\y -> f (p,y)) (s-1) (v+1) z)
  where
  p = (u+r) `div` 2
  q = (v+s) `div` 2
  rfind p = trace ("rfind " ++ show (p,q)) $ (if f (p,q) == z then (p,q): find' (u,v) (p-1, q+1) f z
                           else find' (u,v) (p, q+1) f z) ++
             find' (p+1, q-1) (r,s) f z
  cfind q = trace ("cfind " ++ show (p,q)) $ (find' (u,v) (p-1,q+1) f z) ++
            (if f (p,q) == z then (p,q): find' (p+1, q-1) (r,s) f z
                           else find' (p+1,q) (r,s) f z)

-- invert' ::  ( (Int, Int) -> Int) -> Int -> [(Int, Int)]
invert' f z = find' (0,m) (n,0) f z
  where
    m = bsearch (\y -> f (0,y)) (negate 1) (z+1) z
    n = bsearch (\x -> f (x,0)) (negate 1) (z+1) z
