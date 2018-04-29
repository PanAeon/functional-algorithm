module P04() where

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
