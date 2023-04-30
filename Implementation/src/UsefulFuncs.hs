module UsefulFuncs where

-- From String to Int
rInt :: String -> Int
rInt str = read str :: Int  


-- projections for (a, b, c)
fst :: (a, b, c) -> a
fst (x, _, _) = x

snd :: (a, b, c) -> b
snd (_, y, _) = y

thd :: (a, b, c) -> c
thd (_, _, z) = z

join :: [a] -> [[a]] -> [a]
join _ [] = []
join _ [x]  = x
join sep (x:xs) = x ++ sep ++ join sep xs

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs
