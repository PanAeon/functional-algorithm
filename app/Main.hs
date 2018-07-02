module Main where



data Tree a = Node {
    root   :: a
  , forest :: Forest a
}

type Forest a = [Tree a]

type Callback a = [Forest a] -> [a]

ex1 = [Node 1 [
        Node 2 [Node 5 [], Node 6 []],
        Node 3 [Node 7 [], Node 8 []],
        Node 4 []
      ]]

breadthFirst :: Forest a -> [a]
breadthFirst ts = foldr f b ts []
  where
    f :: Tree a -> ([Forest a] -> [a]) -> ([Forest a] -> [a])
    f (Node x children) fronts = \siblings -> x : fronts (children : siblings)

    b :: [Forest a] -> [a]
    b [] = []
    b qs = foldl (foldr f) b qs []

main :: IO ()
main = putStrLn $ show $ breadthFirst ex1
