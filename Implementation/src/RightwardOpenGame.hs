module RightwardOpenGame where

-- a category of rightward open mean-payoff game
import Data.Set as Set
import UsefulFuncs as Us
import Numeric.Natural
import qualified Data.Vector as V

type Weight = Integer

type Obj = Int

type Edge = V.Vector (V.Vector Int)

data Role
    = Eve
    | Adam
    deriving (Eq, Show)

type ROG = (Obj, Obj, Obj, Edge, V.Vector Role, V.Vector Weight)


id :: Obj -> ROG 
id m = (m, m, 0, V.generate m (\i -> V.singleton (i+m) ), V.empty, V.empty)

moveNodes :: Int -> V.Vector Int -> V.Vector Int 
moveNodes m l = V.map (\i -> i + m) l

mapReplace :: V.Vector Int -> Int -> Int -> V.Vector (V.Vector Int) -> V.Vector Int 
mapReplace l ma mb bl = V.concatMap (\i -> replaceIfExit i ma mb bl) l

replaceIfExit :: Int -> Int -> Int -> V.Vector (V.Vector Int) -> V.Vector Int 
replaceIfExit i ma mb bl = case i < ma of 
                             True -> V.singleton i
                             False -> moveNodes (ma-mb) (bl V.! (i - ma))

scompEa :: Edge -> Edge -> Int -> Int -> Int -> Int -> Int -> Int -> Edge
scompEa ea eb ma na pa mb nb pb = V.map (\l -> V.uniq $ mapReplace l (ma + pa) mb eb ) ea 


scompE :: Edge -> Edge -> Int -> Int -> Int -> Int -> Int -> Int -> Edge
scompE ea eb ma na pa mb nb pb = (scompEa ea eb ma na pa mb nb pb) V.++ (V.map (moveNodes (ma + pa - mb)) (V.drop mb eb))

scomp :: ROG -> ROG -> ROG
scomp (ma, na, pa, ea, ra, wa) (mb, nb, pb, eb, rb, wb) = (ma, nb, pa + pb, scompE ea eb ma na pa mb nb pb, ra V.++ rb, wa V.++ wb) 

moveParaA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int 
moveParaA i ma na pa mb nb pb = case i < ma + pa of 
                                     True  -> i + mb
                                     False -> i + mb + pb 

moveParaAs :: V.Vector Int -> Int -> Int -> Int -> Int -> Int -> Int -> V.Vector Int 
moveParaAs l ma na pa mb nb pb = V.map (\i -> moveParaA i ma na pa mb nb pb) l

pcompEa :: Edge -> Int -> Int -> Int -> Int -> Int -> Int -> Edge
pcompEa ea ma na pa mb nb pb = V.map (\l -> moveParaAs l ma na pa mb nb pb) ea 


moveParaB :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int 
moveParaB i ma na pa mb nb pb = case i < mb + pb of 
                                     True  -> i + ma + pa
                                     False -> i + ma + pa + na 

moveParaBs :: V.Vector Int -> Int -> Int -> Int -> Int -> Int -> Int -> V.Vector Int 
moveParaBs l ma na pa mb nb pb = V.map (\i -> moveParaB i ma na pa mb nb pb) l

pcompEb :: Edge -> Int -> Int -> Int -> Int -> Int -> Int -> Edge
pcompEb ea ma na pa mb nb pb = V.map (\l -> moveParaBs l ma na pa mb nb pb) ea 

constP :: Int -> Edge -> Edge -> Int -> Int -> Int -> Int -> V.Vector Int 
constP i ea eb ma mb pa pb = case i < ma of 
                                  True  -> ea V.! i 
                                  False -> case i < ma + mb of 
                                                True  -> eb V.! (i - ma)
                                                False -> case i < ma + mb + pa of 
                                                              True -> ea V.! (i - mb) 
                                                              False -> eb V.! (i - ma - pa)

pcompE :: Edge -> Edge -> Int -> Int -> Int -> Int -> Int -> Int -> Edge
pcompE ea eb ma na pa mb nb pb = V.generate (ma + mb + pa + pb) (\i -> constP i nea neb ma mb pa pb)
                                 where nea = pcompEa ea ma na pa mb nb pb
                                       neb = pcompEb eb ma na pa mb nb pb

pcomp :: ROG -> ROG -> ROG
pcomp (ma, na, pa, ea, ra, wa) (mb, nb, pb, eb, rb, wb) = (ma + mb, na + nb, pa + pb, pcompE ea eb ma na pa mb nb pb, ra V.++ rb, wa V.++ wb)

reachFrom :: Obj -> Obj -> Obj -> Obj -> Obj -> Obj-> Obj -> Edge -> V.Vector Int
reachFrom l m n ma na pa i ea = case i < ma+pa of 
                                     True  -> V.singleton (i-l)
                                     False -> case i < ma+pa+l of
                                                   True  -> reach l m n ma na pa (i-ma-pa) ea
                                                   False -> V.singleton (i-l-l)

reach :: Obj -> Obj -> Obj -> Obj -> Obj -> Obj -> Obj -> Edge -> V.Vector Int
reach l m n ma na pa i ea = V.uniq $ V.concatMap (\j -> reachFrom l m n ma na pa j ea) (ea V.! i)

traceE :: Obj -> Obj -> Obj -> Obj -> Obj -> Obj -> Edge -> Edge
traceE l m n ma na pa ea = V.generate (ma-l+pa) (\i -> reach l m n ma na pa (i+l) ea)

trace :: Obj -> Obj -> Obj -> ROG -> ROG
trace l m n (ma, na, pa, ea, ra, wa) = (ma - l, na - l, pa, traceE l m n ma na pa ea, ra, wa)


showNode :: Int -> Int -> Int -> String 
showNode i m p = case i < m of 
                      True -> "Entry" ++ show (i)
                      False -> case i < m + p of 
                                    True -> "IPosition" ++ show (i-m)
                                    False -> "Exit"++ show (i-m-p)

showNodes :: Int -> Int -> V.Vector Int -> String 
showNodes m p l = V.foldl (\s -> \i -> s ++ (showNode i m p) ++ ", ") "" l

showROG :: ROG -> String 
showROG (ma, na, pa, ea, ra, wa) =  "Open ends: " ++ show (ma) ++ "\n"
                                 ++ "Exit ends: " ++ show (na) ++ "\n"
                                 ++ "Internal positions:" ++ show (pa) ++ "\n"
                                 ++ "Edges: " ++ V.foldl (\s -> \l -> s ++ "\n" ++ (showNodes ma pa l)) "" ea ++ "\n"
                                 ++ "Roles: " ++ show (ra) ++ "\n"
                                 ++ "Weights: " ++ show (wa) 

-- sym :: Obj -> Obj -> ROG
-- sym m n = (m + n, n + m, 0, Set.fromAscList ([(En i, Ex (i + n)) | i <- [1..m]] ++ [(En (m + j), Ex j) | j <- [1..n]]), [], [])

symB :: Obj -> Obj -> Obj -> V.Vector Int 
symB i m n = case i < m of 
                  True  -> V.singleton (i+n+m+n)
                  False -> V.singleton (i-m+m+n)

sym :: Obj -> Obj -> ROG
sym m n = (m + n, n + m, 0, V.generate (m+n) (\i -> symB i m n), V.empty, V.empty)


-- showROG :: ROG -> String 
-- showROG (ma, na, pa, ea, ra, wa) =  "Open ends: " ++ show (ma) ++ "\n"
--                                  ++ "Exit ends: " ++ show (na) ++ "\n"
--                                  ++ "Internal positions:" ++ show (pa) ++ "\n"
--                                  ++ "Edges: " ++ V.foldl (\s -> \l -> s ++ "\n" ++ (show l)) "" ea ++ "\n"
--                                  ++ "Roles: " ++ show (ra) ++ "\n"
--                                  ++ "Weights: " ++ show (wa) 

 -- isINode :: Node -> Bool
-- isINode (En a)  = False
-- isINode (Ex a) = False
-- isINode (INode a) = True

-- isEn :: Node -> Bool
-- isEn (En a) = True
-- isEn (Ex a) = False
-- isEn (INode a) = False

-- isEx :: Node -> Bool
-- isEx (En a) = False
-- isEx (Ex a) = True
-- isEx (INode a) = False

-- isSame :: Node -> Node -> Bool
-- isSame (Ex m) (En n) = m == n
-- isSame _ _ = False 



-- scompInt :: Int -> Edge -> Edge -> Int -> Int -> Int -> Int -> Int -> Int -> Int 
-- scompInt i ea eb ma na pa mb nb pb = 

-- scompE :: Edge -> Edge -> Int -> Int -> Int -> Int -> Int -> Int -> Edge
-- scompE ea eb ma na pa mb nb pb = V.fromList ([ scompEdge (ea ! i) eb ma pa | i <- [0..(ma+pa-1)] ] ++ [ reind (eb ! i) (ma+pa) | i <- [0..pb-1] ] )
--                                  where lea = V.map (V.toList) ea
--                                        leb = V.map (V.toList) eb
--                                        llea = V.toList lea 
--                                        lleb = V.toList leb 

-- scompE :: Set Edge -> Set Edge -> Set Edge
-- scompE ea eb = Set.fromAscList ([ (i, j) | (i, j) <- la, not (isEx j) ] ++ [ (i, j) | (i, j) <- lb, not (isEn i) ] ++ [ (i, j) | (i, k1) <- la, (k2, j) <- lb, isSame k1 k2 ])
--                where la = Set.toList ea
--                      lb = Set.toList eb

-- reind :: Int -> Int -> Int -> Int -> Int -> Int  
-- reind ma na pa mb nb pb i = 

-- used for scomp
-- reIndE :: Int -> Int -> Int-> Edge -> Edge
-- reIndE pa mb pb e = [ IM.mapWithKey (reind pa mb pb)  e ! i | i <- [0..mb+pb-1] ]
--                     where le = IM.elems e 
-- reIndE m (En a, INode i) = (En a, INode (i + m))
-- reIndE m (En a, Ex b) = (En a, Ex b)
-- reIndE m (INode i, En b) = (INode (i + m), En b)
-- reIndE m (INode i, INode j) = (INode (i + m), INode (j + m))
-- reIndE m (INode i, Ex a) = (INode (i + m), Ex a)
-- reIndE m (Ex a, En b) = (Ex a, En b)
-- reIndE m (Ex a, INode i) = (Ex a, INode (i + m))
-- reIndE m (Ex a, Ex b) = (Ex a, Ex b)

-- used for pcomp
-- reIndN :: Int -> Int -> Int -> Node -> Node 
-- reIndN m p n (En a) = En (m + a)
-- reIndN m p n (INode i) = INode (p + i)
-- reIndN m p n (Ex a) = Ex (n + a) 


-- pcompE :: Int -> Int -> Int -> Edge -> Edge -> Edge
-- pcompE m p n ea eb = Set.fromAscList (la ++ [ (reIndN m p n s1, reIndN m p n s2)| (s1, s2) <- lb ])
--                      where la = Set.toList ea 
--                            lb = Set.toList eb

-- scomp :: ROG -> ROG -> ROG
-- scomp (ma, na, pa, ea, ra, wa) (mb, nb, pb, eb, rb, wb) = (ma, nb, pa + pb, scompE ea eb ma na pa mb nb pb, ra ++ rb, wa ++ wb) 

-- pcomp :: ROG -> ROG -> ROG
-- pcomp (ma, na, pa, ea, ra, wa) (mb, nb, pb, eb, rb, wb) = (ma + mb, na + nb, pa + pb, pcompE ma pa na ea eb, ra ++ rb, wa ++ wb)


-- scompEdge :: Edge -> [Edge] -> Maybe Edge
-- scompEdge (s, Ex i) [(En j, t)] = case i == j of 
--                                        True  -> Just (s, t)
--                                        False -> Nothing
-- scompEdge _ _ = Nothing
                              
-- scompTrace :: Obj -> Set Edge -> Edge -> Maybe Edge
-- scompTrace l s (t, Ex j) = case j <= l of 
--                                 True -> case scompEdge (t, Ex j) e of 
--                                              Just (d1, d2) -> scompTrace l s (d1, d2)
--                                              Nothing       -> Nothing  
--                                         where ls = Set.toList s
--                                               e = [(k1, k2) | (k1, k2) <- ls, k1 == (En j)] 
--                                 False -> Just (t, Ex j)
-- scompTrace l s e         = Just e

-- traceEE :: Obj -> Obj -> Obj -> Set Edge -> Edge -> Maybe Edge
-- traceEE l m n s e = case e of 
--                          (En i, INode j)    -> case i <= l of 
--                                                     True  -> Nothing
--                                                     False -> Just (En i, INode j)
--                          (En i, Ex j)       -> case i <= l of 
--                                                     True  -> Nothing
--                                                     False -> case j <= l of 
--                                                                   True  -> scompTrace l s (En i, Ex j)
--                                                                   False -> Just (En i, Ex j)
--                          (INode i, INode j) -> Just (INode i, INode j)
--                          (INode i, Ex j)    -> case j <= l of 
--                                                     True  -> scompTrace l s (INode i, Ex j)
--                                                     False -> Just (INode i, Ex j)
--                          _                  -> Nothing

-- compile :: [Maybe Edge] -> [Edge]
-- compile []          = []
-- compile ((Just e):xs) = e:(compile xs)
-- compile (Nothing:xs)  = compile xs 

-- -- minus l in En nodes
-- minusLN :: Obj -> Node -> Node 
-- minusLN l (En j) = case j <= l of 
--                         True -> En j
--                         False -> En (j - l)
-- minusLN l (Ex j) = case j <= l of 
--                         True -> Ex j
--                         False -> Ex (j - l)
-- minusLN l n      = n

-- -- minus l in edge
-- minusLE :: Obj -> Edge -> Edge 
-- minusLE l (s, t) = (minusLN l s, minusLN l t)

-- minusL :: Obj -> [Edge] -> [Edge]
-- minusL l [] = []
-- minusL l (x:xs) = (minusLE l x):(minusL l xs)


-- traceE :: Obj -> Obj -> Obj -> Set Edge -> Set Edge
-- traceE l m n s = Set.fromAscList (minusL l (compile [ traceEE l m n s e | e <- ls ]))
--                  where ls = Set.toList s

-- trace :: Obj -> Obj -> Obj -> ROG -> ROG
-- trace l m n (ma, na, pa, ea, ra, wa) = (ma - l, na - l, pa, traceE l m n ea, ra, wa)