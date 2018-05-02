module P04() where

import Data.List
import Data.Vector(Vector)
import qualified Data.Vector as V
import Debug.Trace

_A :: Vector Int
_A = V.fromList [0, 2, 4, 8, 12]
_B :: Vector Int
_B = V.fromList [1, 3, 5, 7, 13]

-- yeah, uneven
kthsmallest :: Int -> Int -> Int -> Vector Int -> Vector Int -> Int
kthsmallest leftA leftB k _A _B =
  if k == 1
  then min (_A V.! leftA) (_B V.! leftB)
  else if _A V.! leftA' > _B V.! leftB'
       then kthsmallest leftA (leftB'+1) (k') _A _B
       else kthsmallest (leftA'+1) leftB (k') _A _B
  where
    z = max 0 $ minimum [((k-1) `div` 2 - (k `mod` 2)), length _A - leftA - 1, length _B - leftB - 1]
    leftA' = leftA + z
    leftB' = leftB + z
    k' = max 1 (k - z - 1)

----------------------- all right -------------

smallest :: Ord a => Int -> ([a], [a]) -> a
smallest k (xs,ys) = union' (xs,ys) !! k

union' :: Ord a => ([a], [a]) -> [a]
union' (xs, []) = xs
union' ([], ys) = ys
union' (x:xs, y:ys)
         | x < y = x : union' (xs, y:ys)
         | x > y = y : union' (x:xs, ys)

smallest' :: Ord a => Int -> ([a], [a]) -> a
smallest' k (xs, []) = xs !! k
smallest' k ([], ys) = ys !! k
smallest' k (zs,ws) =
    case (a < b, k <= p+q) of
      (True, True) -> smallest' k (zs, us)
      (True, False) -> smallest' (k - p - 1) (ys, ws)
      (False, True) -> smallest' k (xs, ws)
      (False, False) -> smallest' (k - q - 1) (zs, vs)
  where
    p = (length zs) `div` 2
    q = (length ws) `div` 2
    (xs, a:ys) = splitAt p zs
    (us, b:vs) = splitAt q ws

-- FIXME: buuuuuuuuuggggggggggssssssssssss
search' :: Ord a => Int -> (Int, Int) -> (Int, Int) -> (Vector a, Vector a) -> a
search' k (lx, rx) (ly, ry) (xa,ya)
   | lx == rx = ya V.! k
   | ly == ry = xa V.! k
   | otherwise =
    trace (">" ++ show k ++ " " ++ show lx ++ " " ++ show rx ++ " " ++ show ly ++ " " ++ show ry)   $
    case (xa V.! mx < ya V.! my, k <= (mx + my)) of
      (True, True) -> search' k (lx, rx) (ly, my) (xa,ya)
      (True, False) -> search' (k - mx - 1) (mx, rx) (ly, ry) (xa,ya)
      (False, True) -> search' k (lx, mx) (ly, ry) (xa,ya)
      (False, False) -> search' (k - my - 1) (lx, rx) (my, ry) (xa,ya)
    where
      mx = (lx + rx) `div` 2
      my = (ly + ry) `div` 2

smallest'' :: Ord a => Int -> (Vector a, Vector a) -> a
smallest'' k (xa, ya) = search' k (0, m) (0, n) (xa, ya)
                       where
                         m = V.length xa
                         n = V.length ya
