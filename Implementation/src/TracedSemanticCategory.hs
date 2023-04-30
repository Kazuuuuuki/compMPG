module TracedSemanticCategory where

import Data.Set as S
import qualified Data.Map.Strict as M
import UsefulFuncs as Us
import qualified KlisliCategory as KC   

type Mor = (KC.Obj, KC.Obj, Set (Set [KC.Codom]))

body :: Mor -> Set (Set [KC.Codom])
body (m, n, s) = s

-- isEqMor :: Mor -> Mor -> Bool
-- isEqMor s t = Us.fst s == Us.fst t && Us.snd s == Us.snd t && (isEqMorBody (Us.fst s) (Us.snd s) (body s) (body t))

-- isEqMorBody :: KC.Obj -> KC.Obj ->  [[KC.Obj -> KC.Codom]] -> [[KC.Obj -> KC.Codom]] -> Bool
-- isEqMorBody m n bs bt = not (elem False [ isEqMorBodyInner m n bs' bt | bs' <- bs]) && not (elem False [ isEqMorBodyInner m n bt' bs | bt' <- bt]) 

-- isEqMorBodyInner :: KC.Obj -> KC.Obj -> [KC.Obj -> KC.Codom] -> [[KC.Obj -> KC.Codom]] -> Bool
-- isEqMorBodyInner m n bs' bt = elem True [ isEqMorBodyInner' m n bs' bt' |bt' <- bt]

-- isEqMorBodyInner' :: KC.Obj -> KC.Obj -> [KC.Obj -> KC.Codom] -> [KC.Obj -> KC.Codom] -> Bool
-- isEqMorBodyInner' m n bs' bt' = not (elem False [ elemForMor m n f bt' |f <- bs']) &&  not (elem False [ elemForMor m n g bs' |g <- bt']) 

-- elemForMor :: KC.Obj -> KC.Obj -> (KC.Obj -> KC.Codom) -> [KC.Obj -> KC.Codom] -> Bool
-- elemForMor m n f bs' = elem True [ KC.isEqMor (m ,n, f) (m, n, g) | g <- bs']

id :: KC.Obj -> Mor
id m = (m, m, S.singleton $ S.singleton $ KC.body (KC.id m))

sym :: KC.Obj -> KC.Obj-> Mor
sym m n = (m+n, n+m, S.singleton$ S.singleton $ KC.body (KC.sym m n))

-- rm_duplicate ::  Mor -> Mor
-- rm_duplicate (m, n, s) = (m, n, rmC_duplicate (m, n, s))

-- rmC_duplicate :: Mor -> [[KC.Obj -> KC.Codom]]
-- rmC_duplicate (m, n, s) = rm_duplicate_funcs m n [ rm_duplicate_func m n s' [] | s' <- s] []

-- rm_duplicate_func :: KC.Obj -> KC.Obj -> [KC.Obj -> KC.Codom] -> [KC.Obj -> KC.Codom] -> [KC.Obj -> KC.Codom]
-- rm_duplicate_func m n [] l = l 
-- rm_duplicate_func m n (x:xs) l = case elemForMor m n x xs of 
--                                       True -> rm_duplicate_func m n xs l
--                                       False -> rm_duplicate_func m n xs (x:l)  

-- rm_duplicate_funcs :: KC.Obj -> KC.Obj -> [[KC.Obj -> KC.Codom]] ->  [[KC.Obj -> KC.Codom]] -> [[KC.Obj -> KC.Codom]]      
-- rm_duplicate_funcs m n [] l = l 
-- rm_duplicate_funcs m n (s':s) l = case isEqMorBodyInner m n s' s of 
--                                        True -> rm_duplicate_funcs m n s l
--                                        False -> rm_duplicate_funcs m n s (s':l)  

getMaxies :: (KC.Obj, KC.Obj, [[KC.Codom]]) -> [[KC.Codom]]
getMaxies (m, n, s) = [ f | f <- s, not (elem True [ KC.isSmallerMor (m, n, f) (m, n, g) | g <- s]) ]

isIncludedOrSmaller :: KC.Obj -> KC.Obj -> [KC.Codom] -> Set [KC.Codom] -> Bool
isIncludedOrSmaller m n f t = elem True [ KC.isSmallOrEqualMor (m, n, f) (m, n, g) | g <- bt]
                              where bt = S.toList $ t


isSmallerOrEqualSetOfMors :: (KC.Obj, KC.Obj, Set [KC.Codom]) -> (KC.Obj, KC.Obj, Set [KC.Codom]) -> Bool
isSmallerOrEqualSetOfMors (m1, n1, s) (m2, n2, t) = (m1 == m2) && (n1 == n2) && not (elem False [ isIncludedOrSmaller m1 n1 f t| f <- bs ])
                                                    where bs = S.toList $ s

getMins :: Mor -> Set (Set [KC.Codom])
getMins (m, n, s) = S.fromAscList [s' | s' <- bs, not (elem True [ isSmallerOrEqualSetOfMors (m, n, t') (m, n, s') && not (isSmallerOrEqualSetOfMors (m, n, s') (m, n, t')) | t' <- bs])]
                    where bs = S.toList $ s


-- -- comp does not check whether f and g is composable.
scomp :: Mor -> Mor -> Mor 
scomp s t = (Us.fst s, Us.snd t, getMins (Us.fst s, Us.snd t, scompC s t))

scompC :: Mor -> Mor -> Set (Set [KC.Codom])
scompC s t = S.fromAscList $ Us.rmdups [S.fromAscList $ Us.rmdups $ getMaxies (m, n, [KC.scompC bf bg | bf <- (S.toList bs'), bg <- (S.toList bt')])| bs' <- bs, bt' <- bt]
-- [ getMaxies (m, n, [KC.scompC bf bg | bf <- (S.fromAscList bs'), bg <- (S.fromAscList bt')])| bs' <- bs, bt' <- bt]
                where bs = S.toList $ body s
                      bt = S.toList $ body t
                      m = Us.fst s
                      n = Us.snd t

pcomp :: Mor -> Mor -> Mor 
pcomp s t = (Us.fst s + Us.fst t, Us.snd s + Us.snd t, pcompC s t)


-- parallel compositon
pcompC :: Mor -> Mor -> Set (Set [KC.Codom])
pcompC s t = S.fromAscList $ Us.rmdups [ S.fromAscList $ Us.rmdups $ [KC.body $ KC.pcomp (m1, n1, bf) (m2, n2, bg) | bf <- (S.toList bs'), bg <- (S.toList bt')]| bs' <- bs, bt' <- bt]
                where bs = S.toList $ body s
                      bt = S.toList $ body t
                      m1 = Us.fst s
                      m2 = Us.fst t
                      n1 = Us.snd s
                      n2 = Us.snd t

-- trace operator
trace :: (KC.Obj, KC.Obj, KC.Obj) -> Mor -> Mor
trace (l, m, n) s = (m, n, getMins (m, n, traceC (l, m, n) s) )


traceC :: (KC.Obj, KC.Obj, KC.Obj) -> Mor -> Set (Set [KC.Codom])
traceC (l, m, n) s = S.fromAscList $ Us.rmdups [ S.fromAscList $ Us.rmdups $ getMaxies (m, n,[KC.traceC (l, m, n) (Us.fst s, Us.snd s, bf) | bf <- (S.toList bs')])| bs' <- bs]
                        where bs = S.toList $ body s
 
printMorInner :: (KC.Obj, KC.Obj, [[KC.Codom]]) -> String -> String 
printMorInner (m, n, []) s = s
printMorInner (m, n, f:s') s = printMorInner (m, n, s') ((KC.printFunc (m, n, f)) ++ "--------\n" ++ s)

recPrintMor :: (KC.Obj, KC.Obj, [[[KC.Codom]]]) -> String -> String
recPrintMor (m, n, []) s = s
recPrintMor (m, n, l':l) s = recPrintMor (m, n, l) ((printMorInner (m, n, l') "") ++ "next eve-strategy \n"++ s)

printMor :: Mor -> String
printMor (m, n, s)  = recPrintMor (m, n, bs) ""
                      where tmp = S.toList s 
                            bs = [ S.toList t | t <- tmp] 

-- checkWhetherWin :: KC.Obj -> [KC.Obj -> KC.Codom] -> Bool
-- checkWhetherWin i [] = True
-- checkWhetherWin i (f:xs) = case f i of 
--                                 KC.EveWin -> checkWhetherWin i xs
--                                 _         -> False

-- solveMor :: Mor -> KC.Obj -> String 
-- solveMor (m, n, []) i = "Not winning" 
-- solveMor (m, n, l:xs) i = case checkWhetherWin i l of 
--                                True  -> "Winning"
--                                False -> solveMor (m, n, xs) i