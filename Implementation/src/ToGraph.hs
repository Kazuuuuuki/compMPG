module ToGraph where 
import qualified SyntaxOfFreeProp as S
import Control.Monad
import qualified Data.Vector as V
import Data.IntMap.Strict as M
import Data.Set as Set
import Interpretation as I
import System.IO
import qualified Data.IntMap.Strict as IntMap
import qualified UsefulFuncs as Us
import qualified Context as C
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as LI
import Data.Monoid


data Label 
    = SComp
    | PComp 
    | Id S.Direction
    | Syn S.Direction S.Direction
    | Unit S.Direction 
    | Counit S.Direction
    | Node S.Role S.Weight S.Obj S.Obj
    deriving (Eq, Show, Read)


data BTree a 
    = DeadEnd a 
    | Branch a (BTree a) (BTree a)
    deriving (Eq, Show, Read)

data Branch a 
    = TurnLeft a (BTree a)
    | TurnRight a (BTree a) 
     deriving (Eq, Show, Read)

data Ob 
    = Dom 
    | Cod
    deriving (Eq, Show, Read)

type Thread a = [Branch a]

type Zipper a = (Thread a, BTree a)

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Branch x l r) = Just (TurnRight x l : t, r)
turnRight _                 = Nothing 

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (t, Branch x l r) = Just (TurnLeft x r : t, l)
turnLeft _                 = Nothing 

back :: Zipper a -> Maybe (Zipper a)
back ([]       , _)         = Nothing 
back (TurnLeft x r : t, l)  = Just (t, Branch x l r)
back (TurnRight x l : t, r) = Just (t, Branch x l r) 

getBT :: BTree a -> a
getBT (DeadEnd x)    = x 
getBT (Branch a _ _) = a

get :: Zipper a -> a 
get (l, DeadEnd x)    = x
get (l, Branch x _ _) = x

put :: a -> Zipper a -> Zipper a
put x (l, DeadEnd y)     = (l, DeadEnd x)
put x (li, Branch y l r ) = (li, Branch x l r)

-- fromPropToBtree :: S.Expr -> BTree Label 
-- fromPropToBtree (S.SComp e1 e2)  = Branch SComp (fromPropToBtree e1) (fromPropToBtree e2)
-- fromPropToBtree (S.MProd e1 e2)  = Branch PComp (fromPropToBtree e1) (fromPropToBtree e2)
-- fromPropToBtree (S.Id d)         = DeadEnd (Id d)
-- fromPropToBtree (S.Syn d1 d2)    = DeadEnd (Syn d1 d2)
-- fromPropToBtree (S.Unit d)       = DeadEnd (Unit d)
-- fromPropToBtree (S.Counit d)     = DeadEnd (Counit d)
-- fromPropToBtree (S.Node r w d c) = DeadEnd (Node r w d c)

fromPropToBtree :: S.Expr -> BTree (Label, Int, Int,  S.Obj, S.Obj) 
fromPropToBtree (S.SComp e1 e2)     = Branch (SComp, lns1 + rns1, lns2 + rns2, dom1, cod2) c1 c2
                                      where c1 = fromPropToBtree e1 
                                            c2 = fromPropToBtree e2 
                                            (_, lns1, rns1, dom1, cod1) = getBT c1
                                            (_, lns2, rns2, dom2, cod2) = getBT c2
fromPropToBtree (S.MProd e1 e2)     = Branch (PComp, lns1 + rns1, lns2 + rns2, dom1 Prelude.++ dom2, cod1 Prelude.++ cod2) c1 c2
                                      where c1 = fromPropToBtree e1 
                                            c2 = fromPropToBtree e2 
                                            (_, lns1, rns1, dom1, cod1) = getBT c1
                                            (_, lns2, rns2, dom2, cod2) = getBT c2
fromPropToBtree (S.Id d)            = DeadEnd (Id d, 0, 0, [d], [d])
fromPropToBtree (S.Syn d1 d2)       = DeadEnd (Syn d1 d2, 0, 0, [d1, d2], [d2, d1])
fromPropToBtree (S.Unit S.DRight)   = DeadEnd (Unit S.DRight, 0, 0, [], [S.DRight, S.DLeft])
fromPropToBtree (S.Unit S.DLeft)    = DeadEnd (Unit S.DLeft, 0, 0, [], [S.DLeft, S.DRight])
fromPropToBtree (S.Counit S.DRight) = DeadEnd (Counit S.DRight, 0, 0, [S.DLeft, S.DRight], [])
fromPropToBtree (S.Counit S.DLeft)  = DeadEnd (Counit S.DLeft, 0, 0, [S.DRight, S.DLeft], [])
fromPropToBtree (S.Node r w d c)    = DeadEnd (Node r w d c, 1, 0, d, c)

data Flag 
    = GoLeft
    | GoRight
    | Back


getNodes :: (Zipper (Label, Int, Int, S.Obj, S.Obj)) -> Int -> Int -> Flag -> IntMap (Zipper (Label, Int, Int, S.Obj, S.Obj), S.Role, S.Weight) -> Maybe (IntMap (Zipper (Label, Int, Int, S.Obj, S.Obj), S.Role, S.Weight))
getNodes ([], DeadEnd (Node r w d c, _, _, dom, cod))                                                       count size _       ans = Just (M.singleton 1 (([], DeadEnd (Node r w d c, 1, 0, dom, cod)), r, w))
getNodes ([], DeadEnd _)                                                                                    count size _       ans = Just M.empty
getNodes (t, Branch (a, i, j, dom, cod) l r)                                                                count size GoLeft  ans = getNodes ( (TurnLeft (a, i, j, dom, cod) r):t, l) count size GoLeft ans 
getNodes (t, Branch (a, i, j, dom, cod) l r)                                                                count size GoRight ans = getNodes ( (TurnRight (a, i, j, dom, cod) l):t, r) count size GoLeft ans 
getNodes ([], Branch (a, i, j, dom, cod) l r)                                                               count size Back    ans = Nothing 
getNodes ((TurnLeft (pa, pi, pj, pdom, pcod) pr):xs, Branch (a, i, j, dom, cod) l r)                        count size Back    ans = getNodes (xs, Branch (pa, pi, pj, pdom, pcod) (Branch (a, i, j, dom, cod) l r) (pr)) count size GoRight ans 
getNodes ((TurnRight (pa, pi, pj, pdom, pcod) pl):xs, Branch (a, i, j, dom, cod) l r)                       count size Back    ans = getNodes (xs, Branch (pa, pi, pj, pdom, pcod) (pl) (Branch (a, i, j, dom, cod) l r)) count size Back ans
getNodes ((TurnLeft (a, pi, pj, pdom, pcod) r):xs, DeadEnd (Node role weight dom cod, i, j, ddom, dcod))    count size _       ans = case count == size of 
                                                                                                                                          True -> Just (M.insert size (((TurnLeft (a, pi, pj, pdom, pcod) r):xs, DeadEnd (Node role weight dom cod, i, j, dom, cod)), role, weight) ans)
                                                                                                                                          False -> getNodes (xs, Branch (a, pi, pj, pdom, pcod) (DeadEnd (Node role weight dom cod, i, j, dom, cod)) r) (count+1) size GoRight nans 
                                                                                                                                          where nans = M.insert count (((TurnLeft (a, pi, pj, pdom, pcod) r):xs, DeadEnd (Node role weight dom cod, i, j, dom, cod)), role, weight) ans
getNodes ((TurnLeft (a, pi, pj, pdom, pcod) r):xs, DeadEnd (b, i, j, dom, cod))                             count size _       ans = getNodes (xs, Branch (a, pi, pj, pdom, pcod) (DeadEnd (b, i, j, dom, cod)) r) count size GoRight ans                                                                                                                                                        
getNodes ((TurnRight (a, pi, pj, pdom, pcod) l):xs, DeadEnd (Node role weight dom cod, i, j, ddom, dcod))   count size _       ans = case count == size of 
                                                                                                                                          True -> Just (M.insert size (((TurnRight (a, pi, pj, pdom, pcod) l):xs, DeadEnd (Node role weight dom cod, i, j, dom, cod)), role, weight) ans)
                                                                                                                                          False -> getNodes ((xs, Branch (a, pi, pj, pdom, pcod) l (DeadEnd (Node role weight dom cod, i, j, dom, cod)))) (count+1) size Back nans 
                                                                                                                                          where nans = M.insert count (((TurnRight (a, pi, pj, pdom, pcod) l):xs, DeadEnd (Node role weight dom cod, i, j, dom, cod)), role, weight) ans      
getNodes ((TurnRight (a, pi, pj, pdom, pcod) l):xs, DeadEnd (b, i, j, dom, cod))                            count size _       ans = getNodes (xs, Branch (a, pi, pj, pdom, pcod) l (DeadEnd (b, i, j, dom, cod))) count size Back ans 


findNode :: Zipper (Label, Int, Int, S.Obj, S.Obj) -> Ob -> S.Direction -> Int ->  Maybe (Zipper (Label, Int, Int, S.Obj, S.Obj))
findNode ([], _)                                                       Cod S.DRight k = Nothing
findNode ((TurnLeft (SComp, i, j, dom, cod) r):xs,  l)                 Cod S.DRight k = findNode ((TurnRight (SComp, i, j, dom, cod) l):xs, r)         Dom S.DRight k
findNode ((TurnLeft (PComp, i, j, dom, cod) r):xs,  l)                 Cod S.DRight k = findNode (xs, Branch (PComp, i, j, dom, cod) l r)              Cod S.DRight k  
findNode ((TurnLeft _ _):xs,                     l)                    Cod S.DRight k = Nothing
findNode ((TurnRight (SComp, i, j, dom, cod) l):xs, r)                 Cod S.DRight k = findNode (xs, Branch (SComp, i, j, dom, cod) l r)              Cod S.DRight k  
findNode ((TurnRight (PComp, i, j, dom, cod) l):xs, r)                 Cod S.DRight k = findNode (xs, Branch (PComp, i, j, dom, cod) l r)              Cod S.DRight (k+(I.numRight y))
                                                                                        where y = case l of 
                                                                                                       DeadEnd (_, _, _, pdom, pcod)        -> pcod
                                                                                                       Branch  (_, _, _, pdom, pcod)  pl pr -> pcod
findNode ((TurnRight _ _):xs,                      l)                  Cod S.DRight j = Nothing 
findNode (l, DeadEnd (Id S.DRight, i, j, dom, cod))                    Dom S.DRight 1 = findNode (l,  DeadEnd (Id S.DRight, i, j, dom, cod))           Cod S.DRight 1
findNode (l, DeadEnd (Syn S.DRight S.DRight, i, j, dom, cod))          Dom S.DRight 1 = findNode (l,  DeadEnd (Syn S.DRight S.DRight, i, j, dom, cod)) Cod S.DRight 2
findNode (l, DeadEnd (Syn S.DRight S.DRight, i, j, dom, cod))          Dom S.DRight 2 = findNode (l,  DeadEnd (Syn S.DRight S.DRight, i, j, dom, cod)) Cod S.DRight 1
findNode (l, DeadEnd (Syn S.DRight S.DLeft, i, j, dom, cod))           Dom S.DRight 1 = findNode (l,  DeadEnd (Syn S.DRight S.DLeft, i, j, dom, cod))  Cod S.DRight 1
findNode (l, DeadEnd (Syn S.DLeft S.DRight, i, j, dom, cod))           Dom S.DRight 1 = findNode (l,  DeadEnd (Syn S.DLeft S.DRight, i, j, dom, cod))  Cod S.DRight 1
findNode (l, DeadEnd (Counit t, i, j, dom, cod))                       Dom S.DRight 1 = findNode (l,  DeadEnd (Counit t, i, j, dom, cod))              Dom S.DLeft 1
findNode (l, DeadEnd (Node r w ndom ncod, i, j, dom, cod))             Dom S.DRight k = Just ((l, DeadEnd (Node r w ndom ncod, i, j, dom, cod)))
findNode (l, Branch (SComp, i, j, dom, cod) t1 t2)                     Dom S.DRight k = findNode ((TurnLeft (SComp, i, j, dom, cod) t2):l, t1) Dom S.DRight k
findNode (l, Branch (PComp, i, j, dom, cod) t1 t2)                     Dom S.DRight k = case k <= (I.numRight y) of 
                                                                                             True  -> findNode ((TurnLeft (PComp, i, j, dom, cod) t2):l, t1) Dom S.DRight k
                                                                                             False -> findNode ((TurnRight (PComp, i, j,dom, cod) t1):l, t2) Dom S.DRight (k - (I.numRight y))
                                                                                        where y = case t1 of 
                                                                                                       DeadEnd (_, _, _, pdom, pcod)       -> pdom
                                                                                                       Branch  (_, _, _, pdom, pcod) pl pr -> pdom
findNode ([], _)                                                       Dom S.DLeft k  = Nothing 
findNode ((TurnLeft (SComp, i, j, dom, cod) r):xs, l)                  Dom S.DLeft k  = findNode (xs, Branch (SComp, i, j, dom, cod) l r)              Dom S.DLeft k 
findNode ((TurnLeft (PComp, i, j, dom, cod) r):xs, l)                  Dom S.DLeft k  = findNode (xs, Branch (PComp, i, j, dom, cod) l r)              Dom S.DLeft (k+(I.numLeft x))
                                                                                        where x = case r of 
                                                                                                       DeadEnd (_, _, _, pdom, pcod)        -> pdom
                                                                                                       Branch  (_, _, _, pdom, pcod)  pl pr -> pdom
findNode ((TurnLeft _ _):xs,                      l)                   Dom S.DLeft k  = Nothing
findNode ((TurnRight (SComp, i, j, dom, cod) l):xs, r)                 Dom S.DLeft k  = findNode ((TurnLeft (SComp, i, j, dom, cod) r):xs, l)          Cod S.DLeft k 
findNode ((TurnRight (PComp, i, j, dom, cod) l):xs, r)                 Dom S.DLeft k  = findNode (xs, Branch (PComp, i, j, dom, cod) l r)              Dom S.DLeft k
findNode ((TurnRight _ _):xs,                      l)                  Dom S.DLeft k  = Nothing 
findNode (l, DeadEnd (Id S.DLeft, i, j, dom, cod))                     Cod S.DLeft 1  = findNode (l, DeadEnd (Id S.DLeft, i, j, dom, cod))             Dom S.DLeft 1 
findNode (l, DeadEnd (Syn S.DLeft S.DLeft, i, j, dom, cod))            Cod S.DLeft 1  = findNode (l, DeadEnd (Syn S.DLeft S.DLeft, i, j, dom, cod))    Dom S.DLeft 2
findNode (l, DeadEnd (Syn S.DLeft S.DLeft, i, j, dom, cod))            Cod S.DLeft 2  = findNode (l, DeadEnd (Syn S.DLeft S.DLeft, i, j, dom, cod))    Dom S.DLeft 1
findNode (l, DeadEnd (Syn S.DRight S.DLeft, i, j, dom, cod))           Cod S.DLeft 1  = findNode (l, DeadEnd (Syn S.DRight S.DLeft, i, j, dom, cod))   Dom S.DLeft 1
findNode (l, DeadEnd (Syn S.DLeft S.DRight, i, j, dom, cod))           Cod S.DLeft 1  = findNode (l, DeadEnd (Syn S.DLeft S.DRight, i, j, dom, cod))   Dom S.DLeft 1
findNode (l, DeadEnd (Unit t, i, j, dom, cod))                         Cod S.DLeft 1  = findNode (l, DeadEnd (Unit t, i, j, dom, cod))                 Cod S.DRight 1
findNode (l, DeadEnd (Node r w ndom ncod, i, j, dom, cod))             Cod S.DLeft k  = Just ((l, DeadEnd (Node r w ndom ncod, i, j, dom, cod)))
findNode (l, Branch (SComp, i, j, dom, cod) t1 t2)                     Cod S.DLeft k  = findNode ((TurnRight (SComp, i, j, dom, cod) t1):l, t2) Cod S.DLeft k 
findNode (l, Branch (PComp, i, j,dom, cod) t1 t2)                      Cod S.DLeft k  = case k <= (I.numLeft y) of 
                                                                                             True  -> findNode ((TurnRight (PComp, i, j, dom, cod) t1):l, t2) Cod S.DLeft k
                                                                                             False -> findNode ((TurnLeft (PComp, i, j, dom, cod) t2):l, t1) Cod S.DLeft (k - (I.numLeft y))
                                                                                        where y = case t2 of 
                                                                                                      DeadEnd (_, _, _, pdom, pcod)       -> pcod
                                                                                                      Branch  (_, _, _, pdom, pcod) pl pr -> pcod
findNode _                                                             _   _       _  = Nothing


-- findNode :: Zipper (Label, Int, Int, S.Obj, S.Obj) -> Ob -> S.Direction -> Int ->  Maybe (Zipper (Label, Int, Int, S.Obj, S.Obj))
-- findNode ([], _)                                                       Cod S.DRight k = Nothing
-- findNode ((TurnLeft (SComp, i, j, dom, cod) r):xs,  l)                 Cod S.DRight k = findNode ((TurnRight (SComp, i, j, dom, cod) l):xs, r)         Dom S.DRight k
-- findNode ((TurnLeft (PComp, i, j, dom, cod) r):xs,  l)                 Cod S.DRight k = findNode (xs, Branch (PComp, i, j, dom, cod) l r)              Cod S.DRight k  
-- findNode ((TurnLeft _ _):xs,                     l)                    Cod S.DRight k = Nothing
-- findNode ((TurnRight (SComp, i, j, dom, cod) l):xs, r)                 Cod S.DRight k = findNode (xs, Branch (SComp, i, j, dom, cod) l r)              Cod S.DRight k  
-- findNode ((TurnRight (PComp, i, j, dom, cod) l):xs, r)                 Cod S.DRight k = findNode (xs, Branch (PComp, i, j, dom, cod) l r)              Cod S.DRight (k+(I.numRight y))
--                                                                                         where y = case l of 
--                                                                                                        DeadEnd (_, _, _, pdom, pcod)        -> pcod
--                                                                                                        Branch  (_, _, _, pdom, pcod)  pl pr -> pcod
-- findNode ((TurnRight _ _):xs,                      l)                  Cod S.DRight j = Nothing 
-- findNode (l, DeadEnd (Id S.DRight, i, j, dom, cod))                    Dom S.DRight 1 = findNode (l,  DeadEnd (Id S.DRight, i, j, dom, cod))           Cod S.DRight 1
-- findNode (l, DeadEnd (Syn S.DRight S.DRight, i, j, dom, cod))          Dom S.DRight 1 = findNode (l,  DeadEnd (Syn S.DRight S.DRight, i, j, dom, cod)) Cod S.DRight 2
-- findNode (l, DeadEnd (Syn S.DRight S.DRight, i, j, dom, cod))          Dom S.DRight 2 = findNode (l,  DeadEnd (Syn S.DRight S.DRight, i, j, dom, cod)) Cod S.DRight 1
-- findNode (l, DeadEnd (Syn S.DRight S.DLeft, i, j, dom, cod))           Dom S.DRight 1 = findNode (l,  DeadEnd (Syn S.DRight S.DLeft, i, j, dom, cod))  Cod S.DRight 1
-- findNode (l, DeadEnd (Syn S.DLeft S.DRight, i, j, dom, cod))           Dom S.DRight 1 = findNode (l,  DeadEnd (Syn S.DLeft S.DRight, i, j, dom, cod))  Cod S.DRight 1
-- findNode (l, DeadEnd (Counit t, i, j, dom, cod))                       Dom S.DRight 1 = findNode (l,  DeadEnd (Counit t, i, j, dom, cod))              Dom S.DLeft 1
-- findNode (l, DeadEnd (Node r w ndom ncod, i, j, dom, cod))             Dom S.DRight k = Just ((l, DeadEnd (Node r w ndom ncod, i, j, dom, cod)))
-- findNode (l, Branch (SComp, i, j, dom, cod) t1 t2)                     Dom S.DRight k = findNode ((TurnLeft (SComp, i, j, dom, cod) t2):l, t1) Dom S.DRight k
-- findNode (l, Branch (PComp, i, j, dom, cod) t1 t2)                     Dom S.DRight k = case k <= (I.numRight y) of 
--                                                                                              True  -> findNode ((TurnLeft (PComp, i, j, dom, cod) t2):l, t1) Dom S.DRight k
--                                                                                              False -> findNode ((TurnRight (PComp, i, j,dom, cod) t1):l, t2) Dom S.DRight (k - (I.numRight y))
--                                                                                         where y = case t1 of 
--                                                                                                        DeadEnd (_, _, _, pdom, pcod)       -> pdom
--                                                                                                        Branch  (_, _, _, pdom, pcod) pl pr -> pdom
-- findNode ([], _)                                                       Dom S.DLeft k  = Nothing 
-- findNode ((TurnLeft (SComp, i, j, dom, cod) r):xs, l)                  Dom S.DLeft k  = findNode (xs, Branch (SComp, i, j, dom, cod) l r)              Dom S.DLeft k 
-- findNode ((TurnLeft (PComp, i, j, dom, cod) r):xs, l)                  Dom S.DLeft k  = findNode (xs, Branch (PComp, i, j, dom, cod) l r)              Dom S.DLeft k
-- findNode ((TurnLeft _ _):xs,                      l)                   Dom S.DLeft k  = Nothing
-- findNode ((TurnRight (SComp, i, j, dom, cod) l):xs, r)                 Dom S.DLeft k  = findNode ((TurnLeft (SComp, i, j, dom, cod) r):xs, l)          Cod S.DLeft k 
-- findNode ((TurnRight (PComp, i, j, dom, cod) l):xs, r)                 Dom S.DLeft k  = findNode (xs, Branch (PComp, i, j, dom, cod) l r)              Dom S.DLeft (k+(I.numLeft y))
--                                                                                         where y = case l of 
--                                                                                                        DeadEnd (_, _, _, pdom, pcod)        -> pdom
--                                                                                                        Branch  (_, _, _, pdom, pcod)  pl pr -> pdom
-- findNode ((TurnRight _ _):xs,                      l)                  Dom S.DLeft k  = Nothing 
-- findNode (l, DeadEnd (Id S.DLeft, i, j, dom, cod))                     Cod S.DLeft 1  = findNode (l, DeadEnd (Id S.DLeft, i, j, dom, cod))             Dom S.DLeft 1 
-- findNode (l, DeadEnd (Syn S.DLeft S.DLeft, i, j, dom, cod))            Cod S.DLeft 1  = findNode (l, DeadEnd (Syn S.DLeft S.DLeft, i, j, dom, cod))    Dom S.DLeft 2
-- findNode (l, DeadEnd (Syn S.DLeft S.DLeft, i, j, dom, cod))            Cod S.DLeft 2  = findNode (l, DeadEnd (Syn S.DLeft S.DLeft, i, j, dom, cod))    Dom S.DLeft 1
-- findNode (l, DeadEnd (Syn S.DRight S.DLeft, i, j, dom, cod))           Cod S.DLeft 1  = findNode (l, DeadEnd (Syn S.DRight S.DLeft, i, j, dom, cod))   Dom S.DLeft 1
-- findNode (l, DeadEnd (Syn S.DLeft S.DRight, i, j, dom, cod))           Cod S.DLeft 1  = findNode (l, DeadEnd (Syn S.DLeft S.DRight, i, j, dom, cod))   Dom S.DLeft 1
-- findNode (l, DeadEnd (Unit t, i, j, dom, cod))                         Cod S.DLeft 1  = findNode (l, DeadEnd (Unit t, i, j, dom, cod))                 Cod S.DRight 1
-- findNode (l, DeadEnd (Node r w ndom ncod, i, j, dom, cod))             Cod S.DLeft k  = Just ((l, DeadEnd (Node r w ndom ncod, i, j, dom, cod)))
-- findNode (l, Branch (SComp, i, j, dom, cod) t1 t2)                     Cod S.DLeft k  = findNode ((TurnRight (SComp, i, j, dom, cod) t1):l, t2) Cod S.DLeft k 
-- findNode (l, Branch (PComp, i, j,dom, cod) t1 t2)                      Cod S.DLeft k  = case k <= (I.numLeft y) of 
--                                                                                              True  -> findNode ((TurnLeft (PComp, i, j, dom, cod) t2):l, t1) Cod S.DLeft k
--                                                                                              False -> findNode ((TurnRight (PComp, i, j, dom, cod) t1):l, t2) Cod S.DLeft (k - (I.numLeft y))
--                                                                                        where y = case t1 of 
--                                                                                                       DeadEnd (_, _, _, pdom, pcod)       -> pcod
--                                                                                                       Branch  (_, _, _, pdom, pcod) pl pr -> pcod
-- findNode _                                                             _   _       _  = Nothing


countNodes :: Thread (Label, Int, Int, S.Obj, S.Obj) -> Int 
countNodes []                               = 1
countNodes ((TurnLeft (a, i, j, dom, cod) r):xs)   = countNodes xs
countNodes ((TurnRight (a, i, j, dom, cod) r):xs)  = i + (countNodes xs)
-- countNodes (TurnRight (a, i, j, d, c) e):xs = i + (countNodes xs)


indNode :: Zipper (Label, Int, Int, S.Obj, S.Obj) -> Int
indNode (l, _) = countNodes l


init :: BTree a -> Zipper a 
init b = ([], b)

numOfNodes :: BTree (Label, Int, Int, S.Obj, S.Obj) -> Int 
numOfNodes (DeadEnd (Node _ _ _ _, _, _, _, _)) = 1
numOfNodes (DeadEnd _) = 0
numOfNodes (Branch _ l r)  = (numOfNodes l) + (numOfNodes r)

 
getEdgeWithNode :: Zipper (Label, Int, Int, S.Obj, S.Obj) -> Ob -> S.Direction -> Int -> Int
getEdgeWithNode m op d i = case mz of 
                                Just z  -> indNode z
                                Nothing -> (-1) 
                           where mz = findNode m op d i 
                        

getEdgesWithNode :: Zipper (Label, Int, Int, S.Obj, S.Obj) -> Ob -> S.Direction -> Int -> Int -> Set Int -> Set Int
getEdgesWithNode z op d i s l = case i < s of
                                     True  -> Set.insert (getEdgeWithNode z op d i) (getEdgesWithNode z op d (i+1) s l) 
                                     False -> case s of 
                                                   0 -> l
                                                   _ -> Set.insert (getEdgeWithNode z op d i) l


getEdges :: IntMap (Zipper (Label, Int, Int, S.Obj, S.Obj), S.Role, S.Weight) -> Int -> Int -> IntMap (Set Int)
getEdges m i s =  case i < s of 
                       True  -> M.insert i ans (getEdges m (i+1) s)
                       False -> case s of 
                                     0 -> M.empty
                                     _ -> M.singleton i ans
                  where ((l, bt), r, w)     = m M.! i
                        (_, _, _, dom, cod) = getBT bt
                        ndom                = I.numLeft dom 
                        ncod                = I.numRight  cod 
                        ans                 = getEdgesWithNode (l, bt) Cod S.DRight 1 ncod (getEdgesWithNode (l, bt) Dom S.DLeft 1 ndom Set.empty)  


getRoleAndWeights :: IntMap (Zipper (Label, Int, Int, S.Obj, S.Obj), S.Role, S.Weight) -> Int -> Int  -> IntMap (S.Role, S.Weight)
getRoleAndWeights m i s = case i <= s of 
                               True  -> M.insert i (r, w) (getRoleAndWeights m (i + 1) s)
                               False -> case s of 
                                             0 -> M.empty
                                             _ -> M.singleton 1 (r, w)
                               where (_, r, w) = (m M.! i)


fromBTreeToGraph :: S.Expr -> (Int, IntMap (Set Int), IntMap (S.Role, S.Weight))
fromBTreeToGraph b = (n, edges, rolesandweights)
                      where bt    = fromPropToBtree b 
                            n     = numOfNodes bt 
                            z     = ToGraph.init bt 
                            nodes = case getNodes z 1 n GoLeft M.empty of 
                                         Just t  -> t
                                         Nothing -> M.empty
                            edges = getEdges nodes 1 n
                            rolesandweights = getRoleAndWeights nodes 1 n 

fromRoleToInt :: S.Role -> String 
fromRoleToInt S.Eve = "0"
fromRoleToInt S.Adam = "1"

-- if the node is deadend (means that the string argument is empty), we add an edge into dummy nodes.
deadEnd :: Int -> S.Role -> String 
deadEnd i S.Eve  = show (i+1)
deadEnd i S.Adam = show (i+2)

-- descriptNode :: Int -> (n, [Set Int], [(S.Role, S.Weight)]) -> String 
-- descriptNode i (n, es, l) = case i <= n of 
--                                          True  -> show (i) ++ " " ++ (show ()) ++ " " ++ (fromRoleToInt (S.Eve)) ++ " " ++ (succersors i (m, n, p, e, r, w)) ++ " " ++ "\"R.En" ++ show (i+1) ++ "\";" ++ "\n" 
--                                                   ++ (descriptNode (i+1) (m, n, p, e, r, w))
--                                          False -> case i < m + p of
--                                                        True -> show (i) ++ " " ++ (show (w V.! (i - m))) ++ " " ++ (fromRoleToInt (r V.! (i - m))) ++ " " ++ (isDeadEnd (m+n+p) (r V.! (i - m)) (succersors i (m, n, p, e, r, w))) ++ " " ++ "\"R.INode"++show (i - m + 1)++ "\";" ++ "\n" 
--                                                                ++ (descriptNode (i+1) (m, n, p, e, r, w))
--                                                        False -> case i < m + p + n of 
--                                                                      True -> show (i) ++ " " ++ (show (-1)) ++ " " ++ (fromRoleToInt (S.Eve)) ++ " " ++ "emp" ++ " " ++ "\"R.Ex" ++show (i - m - p + 1) ++ "\";" ++ "\n" 
--                                                                              ++ (descriptNode (i+1) (m, n, p, e, r, w))
--       
                                                            --    False -> ""

succersors :: Set Int -> Int -> S.Role -> String 
succersors s n r = case st of 
                        ""  -> deadEnd n r 
                        _   -> tail st
                 where st = Set.foldr (\i -> \f -> f++","++(show i)) "" s 

descriptNode :: Int -> (Int, IntMap (Set Int), IntMap (S.Role, S.Weight)) -> String 
descriptNode i (n, es, l) = case i <= n of 
                                 False -> ""
                                 True  -> (show i) ++  " " ++ (show $ snd (l M.! i)) ++ " " ++ (fromRoleToInt $ fst (l M.! i)) ++ " " ++ (succersors (es M.! i) n (fst (l M.! i))) ++ " " ++ "\"Node" ++ (show i) ++ "\";" ++ "\n" ++ (descriptNode (i+1) (n, es, l) )

descriptNodeBuilder :: Int -> (Int, IntMap (Set Int), IntMap (S.Role, S.Weight)) -> Builder 
descriptNodeBuilder i (n, es, l) = case i <= n of 
                                        False -> fromString ""
                                        True  -> (fromString $ show i) <>  (fromString " ") <> (fromString $ show $ snd (l M.! i)) <> (fromString " ") <> (fromString $ fromRoleToInt $ fst (l M.! i)) <> (fromString " ") <> (fromString $ succersors (es M.! i) n (fst (l M.! i))) <> (fromString " ") <> (fromString "\"Node") <> (fromString $ show i) <> (fromString "\";") <> (fromString "\n") <> (descriptNodeBuilder (i+1) (n, es, l) )



-- fromGraphToString :: (Int, IntMap (Set Int), IntMap (S.Role, S.Weight)) -> String
-- fromGraphToString (n, es, l) =  "parity " ++ show (n + 3) ++ ";"++ "\n" 
--                              ++ "0" ++ " " ++ "0" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";" ++ "\n"
--                              ++ descriptNode 1 (n, es, l)
--                              ++ (show (n+1)) ++ " " ++ "-1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (n+1)) ++ " " ++ "\"DummyAdamWin"++ "\";" ++ "\n"
--                              ++ (show (n+2)) ++ " " ++ "1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (n+2)) ++ " " ++ "\"DummyEveWin"++ "\";" ++ "\n"
--                             --  ++ descriptNode 


fromGraphToBuilder :: (Int, IntMap (Set Int), IntMap (S.Role, S.Weight)) -> Builder
fromGraphToBuilder (n, es, l) =  (fromString "parity ") <> (fromString $ show (n + 3)) <> (fromString ";") <> (fromString "\n") 
                              <> (fromString "0") <> (fromString " ") <> (fromString "0") <> (fromString " ") <> (fromString $ fromRoleToInt S.Eve) <> (fromString " ") <> (fromString "1") <> (fromString " ") <> (fromString "\"En") <> (fromString "1") <> (fromString "\";") <> (fromString "\n")
                              <> descriptNodeBuilder 1 (n, es, l)
                              <> (fromString $ show (n+1)) <> (fromString " ") <> (fromString "-1") <> (fromString " ") <> (fromString $ fromRoleToInt S.Eve) <> (fromString " ") <> (fromString $ show (n+1)) <> (fromString " ") <> (fromString "\"DummyAdamWin") <> (fromString "\";") <> (fromString "\n")
                              <> (fromString $ show (n+2)) <> (fromString " ") <> (fromString "1")  <> (fromString " ") <> (fromString $ fromRoleToInt S.Eve) <> (fromString " ") <> (fromString $ show (n+2)) <> (fromString " ") <> (fromString "\"DummyEveWin")  <> (fromString "\";") <> (fromString "\n")
                            --  ++ descriptNode 

appendNode :: Int -> FilePath-> (Int, IntMap (Set Int), IntMap (S.Role, S.Weight)) -> IO ()
appendNode i path (n, es, l) = case i <= n of 
                                    False -> putStrLn "finish! Congrats!" 
                                    True  -> do LI.appendFile path (L.pack $ (show i) ++  " " ++ (show $ snd (l M.! i)) ++ " " ++ (fromRoleToInt $ fst (l M.! i)) ++ " " ++ (succersors (es M.! i) n (fst (l M.! i))) ++ " " ++ "\"Node" ++ (show i) ++ "\";" ++ "\n" ++ (descriptNode (i+1) (n, es, l) ) )
                                                appendNode (i+1) path (n, es, l)

writeGraphInFile :: Handle -> Int -> (Int, IntMap (Set Int), IntMap (S.Role, S.Weight)) -> IO ()
writeGraphInFile  h i (n, es, l) = case i <= n of 
                                        False -> putStrLn "finish nodes"
                                        True  -> do LI.hPutStrLn h (L.pack ((show i) ++  " " ++ (show $ snd (l M.! i)) ++ " " ++ (fromRoleToInt $ fst (l M.! i)) ++ " " ++ (succersors (es M.! i) n (fst (l M.! i))) ++ " " ++ "\"Node" ++ (show i) ++ "\";"))
                                                    writeGraphInFile  h (i+1) (n, es, l)

writeGraphs :: Int -> IO ()
writeGraphs 1 = writeGraph 1 
writeGraphs n = do writeGraph n 
                   writeGraphs (n-1) 


writeGraph :: Int -> IO ()
writeGraph n = do     let path = "contexts/input" ++ (show n) ++ ".txt"
                      handle <- openFile path ReadMode 
                      contents <- hGetContents handle
                      let l = lines contents
                      let c = read $ head l :: C.Context 
                      let sc = read $ head $ tail l :: Int 
                      let m = read $ head $ tail $ tail l :: Int
                      let ll = read $ head $ tail $ tail $ tail l :: Int
                      let s = Prelude.map read (tail $ tail $ tail $ tail l)
                      let ce = C.translation (c, s)
                      let e = case sc of 
                                   1       -> powStep m (C.gsc1 ll) ce
                                   _       -> ce
                      let f1 = S.MProd (S.Id S.DRight) (S.Unit S.DLeft)
                      let f2 = S.MProd e (S.Id S.DRight)
                      let f3 = S.MProd (S.Id S.DRight) (S.Counit S.DRight)
                      let g  = S.SComp (S.SComp f1 f2) f3
                      let g1 = S.SComp (S.Node S.Eve 0 [S.DRight, S.DRight] [S.DRight]) g
                      let g2 = S.MProd (S.Id S.DRight) (S.Unit S.DRight)
                      let g3 = S.MProd (g1) (S.Id S.DLeft)
                      let g4 = S.Counit S.DLeft
                      let a  = S.SComp (S.SComp g2 g3) g4
                      let tmp = fromBTreeToGraph a
                      --  putStrLn $ show $ tmp
                      --  putStrLn $ "finish showing the graph!!"
                      let (num, es, rw) = tmp
                      putStrLn $ show num
                      -- let s1 = L.pack $ "parity " ++ show (num + 3) ++ ";"++ "\n" ++ "0" ++ " " ++ "0" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";" ++ "\n"
                      let path2 = "pgs/input" ++ (show n) ++ ".txt"
                      h <- openFile path2 WriteMode
                      LI.hPutStrLn h (L.pack ("parity " ++ show (num + 3) ++ ";"))
                      LI.hPutStrLn h  (L.pack ("0" ++ " " ++ "0" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";"))
                      writeGraphInFile h 1 tmp
                      LI.hPutStrLn h (L.pack ((show (num+1)) ++ " " ++ "-1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (num+1)) ++ " " ++ "\"DummyAdamWin"++ "\";"))
                      LI.hPutStrLn h (L.pack ((show (num+2)) ++ " " ++ "1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (num+2)) ++ " " ++ "\"DummyEveWin"++ "\";"))
                      hClose h



-- writeGraph :: Int -> IO ()
-- writeGraph n = do     let path = "contexts/input" ++ (show n) ++ ".txt"
--                       handle <- openFile path ReadMode 
--                       contents <- hGetContents handle
--                       let l = lines contents
--                       let c = read $ head l :: C.Context 
--                       let sc = read $ head $ tail l :: String 
--                       let m = read $ head $ tail $ tail l :: Int
--                       let s = Prelude.map read (tail $ tail $ tail l)
--                       let ce = C.translation (c, s)
--                       let e = case sc of 
--                                    "case1" -> powStep m C.sc5 ce
--                                    _       -> ce
--                       let f1 = S.MProd (S.Id S.DRight) (S.Unit S.DLeft)
--                       let f2 = S.MProd e (S.Id S.DRight)
--                       let f3 = S.MProd (S.Id S.DRight) (S.Counit S.DRight)
--                       let g  = S.SComp (S.SComp f1 f2) f3
--                       let g1 = S.SComp (S.Node S.Eve 0 [S.DRight, S.DRight] [S.DRight]) g
--                       let g2 = S.MProd (S.Id S.DRight) (S.Unit S.DRight)
--                       let g3 = S.MProd (g1) (S.Id S.DLeft)
--                       let g4 = S.Counit S.DLeft
--                       let a  = S.SComp (S.SComp g2 g3) g4
--                       let tmp = fromBTreeToGraph a
--                       --  putStrLn $ show $ tmp
--                       --  putStrLn $ "finish showing the graph!!"
--                       let (num, es, rw) = tmp
--                       putStrLn $ show num
--                       -- let s1 = L.pack $ "parity " ++ show (num + 3) ++ ";"++ "\n" ++ "0" ++ " " ++ "0" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";" ++ "\n"
--                       let path2 = "pgst/input" ++ (show n) ++ ".txt"
--                       h <- openFile path2 WriteMode
--                       LI.hPutStrLn h (L.pack ("parity " ++ show (num + 3) ++ ";"))
--                       LI.hPutStrLn h  (L.pack ("0" ++ " " ++ "0" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";"))
--                       writeGraphInFile h 1 tmp
--                       LI.hPutStrLn h (L.pack ((show (num+1)) ++ " " ++ "-1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (num+1)) ++ " " ++ "\"DummyAdamWin"++ "\";"))
--                       LI.hPutStrLn h (L.pack ((show (num+2)) ++ " " ++ "1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (num+2)) ++ " " ++ "\"DummyEveWin"++ "\";"))
--                       hClose h

powStep :: Int -> (Int -> C.StepContext) -> S.Expr -> S.Expr
powStep 1 _  s = s
powStep n fs s = powStep (n-1) fs (C.stepTranslation (fs n, s))


-- writeGraph :: Int -> IO ()
-- writeGraph n = do let path = "contexts/input" ++ (show n) ++ ".txt"
--                   handle <- openFile path ReadMode 
--                   contents <- hGetContents handle
--                   let l = lines contents
--                   let c = read $ head l :: C.Context 
--                   let s = Prelude.map read (tail l)
--                   let e = C.translation (c, s)
--                   let f1 = S.MProd (S.Id S.DRight) (S.Unit S.DLeft)
--                   let f2 = S.MProd e (S.Id S.DRight)
--                   let f3 = S.MProd (S.Id S.DRight) (S.Counit S.DRight)
--                   let g  = S.SComp (S.SComp f1 f2) f3
--                   let g1 = S.SComp (S.Node S.Eve 0 [S.DRight, S.DRight] [S.DRight]) g
--                   let g2 = S.MProd (S.Id S.DRight) (S.Unit S.DRight)
--                   let g3 = S.MProd (g1) (S.Id S.DLeft)
--                   let g4 = S.Counit S.DLeft
--                   let a  = S.SComp (S.SComp g2 g3) g4
--                   let tmp = fromBTreeToGraph a
--                   putStrLn $ show $ tmp
--                   putStrLn $ "finish showing the graph!!"
--                   let (num, es, rw) = tmp
--                   -- let s1 = L.pack $ "parity " ++ show (num + 3) ++ ";"++ "\n" ++ "0" ++ " " ++ "0" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";" ++ "\n"
--                   let path2 = "pgst/input" ++ (show n) ++ ".txt"
--                   h <- openFile path2 WriteMode
--                   LI.hPutStrLn h (L.pack ("parity " ++ show (num + 3) ++ ";"))
--                   LI.hPutStrLn h  (L.pack ("0" ++ " " ++ "0" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";"))
--                   writeGraphInFile h 1 tmp
--                   LI.hPutStrLn h (L.pack ((show (num+1)) ++ " " ++ "-1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (num+1)) ++ " " ++ "\"DummyAdamWin"++ "\";"))
--                   LI.hPutStrLn h (L.pack ((show (num+2)) ++ " " ++ "1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (num+2)) ++ " " ++ "\"DummyEveWin"++ "\";"))
--                   hClose h
                  -- let s2 = L.pack $ (show (num+1)) ++ " " ++ "-1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (num+1)) ++ " " ++ "\"DummyAdamWin"++ "\";" ++ "\n" ++ (show (num+2)) ++ " " ++ "1" ++ " " ++ (fromRoleToInt S.Eve) ++ " " ++ (show (num+2)) ++ " " ++ "\"DummyEveWin"++ "\";" ++ "\n"
                  -- LI.appendFile path2 s2 

translateFiles :: Int -> IO ()
translateFiles 0 = putStrLn $ "finished creating mean payoff games!"
translateFiles n = do let path = "contexts/input" ++ (show n) ++ ".txt"
                      handle <- openFile path ReadMode 
                      contents <- hGetContents handle
                      let l = lines contents
                      let c = read $ head l :: C.Context 
                      let s = Prelude.map read (tail l)
                      let e = C.translation (c, s)
                      let f1 = S.MProd (S.Id S.DRight) (S.Unit S.DLeft)
                      let f2 = S.MProd e (S.Id S.DRight)
                      let f3 = S.MProd (S.Id S.DRight) (S.Counit S.DRight)
                      let g  = S.SComp (S.SComp f1 f2) f3
                      let g1 = S.SComp (S.Node S.Eve 0 [S.DRight, S.DRight] [S.DRight]) g
                      let g2 = S.MProd (S.Id S.DRight) (S.Unit S.DRight)
                      let g3 = S.MProd (g1) (S.Id S.DLeft)
                      let g4 = S.Counit S.DLeft
                      let a  = S.SComp (S.SComp g2 g3) g4
                      let tmp = fromBTreeToGraph a
                      putStrLn $ show $ Us.fst $ tmp 
                      let mg = toLazyText $ fromGraphToBuilder $ tmp
                    --   putStrLn $ show $ length mg 
                      let path2 = "pgst/input" ++ (show n) ++ ".txt"
                      LI.writeFile path2 mg
                    --   writeFile path2 (show tmp)
                      putStrLn $ "finish" ++ (show n)
                      translateFiles (n-1)


translateFilesTest :: Int -> IO ()
translateFilesTest 0 = putStrLn $ "finished creating mean payoff games!"
translateFilesTest n = do let path = "contexts/input" ++ (show n) ++ ".txt"
                          handle <- openFile path ReadMode 
                          contents <- hGetContents handle
                          let l = lines contents
                          let c = read $ head l :: C.Context 
                          let s = Prelude.map read (tail l)
                          let e = C.translation (c, s)
                          let f1 = S.MProd (S.Id S.DRight) (S.Unit S.DLeft)
                          let f2 = S.MProd e (S.Id S.DRight)
                          let f3 = S.MProd (S.Id S.DRight) (S.Counit S.DRight)
                          let g  = S.SComp (S.SComp f1 f2) f3
                          let g1 = S.SComp (S.Node S.Eve 0 [S.DRight, S.DRight] [S.DRight]) g
                          let g2 = S.MProd (S.Id S.DRight) (S.Unit S.DRight)
                          let g3 = S.MProd (g1) (S.Id S.DLeft)
                          let g4 = S.Counit S.DLeft
                          let a  = S.SComp (S.SComp g2 g3) g4
                          let tmp = fromBTreeToGraph a
                          putStrLn $ show $ a
                          translateFilesTest (n-1)
-- fromBTreeToGraph b = (numOfNodes b, edges b, roleOfNodes, weightOfNodes)