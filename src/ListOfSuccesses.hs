module ListOfSuccesses() where


-- FROM replacing "failure with a list of successes"
-- by Philip Wadler

-- as usual need another repetition

 
assoc :: Eq a => [(a,b)] -> a -> [b]
assoc [] _ = []
assoc ((x,y):xs) z
      | x == z    = y:(assoc xs z)
      | otherwise = assoc xs z

assoc' xys x= [y | (x',y) <- xys, x == x']

assoc'' xys x = map snd (filter ((==x) . fst) xys)

or' = (++)

and' f xs ys = [v1 `f` v2 | v1 <- xs, v2 <- ys] -- must be fair

cut [] = []
cut (x:xs) = [x]

lit :: (Eq a) => a -> [a] -> [(a, [a])]
lit x [] = []
lit x (x':xs) | x == x'   = [(x,xs)]
              | otherwise = []

empty :: a -> String -> [(a,String)]
empty v xs = [(v,xs)]
fail' :: String -> [(a,String)]
fail' xs = []

alt :: Parser a -> Parser a -> Parser a
alt p q xs = (p xs) ++ (q xs)

type Parser a = String -> [(a,String)]

seq' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
seq' f p q xs = [ (f v1 v2, bs) | (v1, as) <- p xs, (v2, bs) <- q as]

rep :: Parser a -> Parser [a]
rep p = alt (seq' (:) p (rep p)) (empty [])

rep1 :: Parser a -> Parser [a]
rep1 p = seq' (:) p (rep p)

alts ps = foldr alt fail' ps
seqs ps = foldr (seq' (:)) (empty []) ps
lits xs = seqs [ lit x | x <- xs]

expr :: Parser String
expr = alts [number, fix $ seqs [lits "(", expr, op, expr, lits ")"]]
   where
     fix p = \s -> fmap (\t -> (foldr (++) "" (fst t), snd t))  (p s)
number :: Parser String
number = rep1 digit
op :: Parser String
op = alts [lits "+", lits "-", lits "*", lits "/"]
digit :: Parser Char
digit = alts [lit x | x <- ['0'..'9']]


app f p xs = [(f v, xs') | (v, xs') <- p xs]

mknmber :: String -> Int
mknmber = read

number' = app mknmber (rep1 digit)

alt' p q = cut . alt p q

seq'' f p q = seq f p (nofail . q)

nofail u = (fst (head u), snd (head u)) : (tail u)
