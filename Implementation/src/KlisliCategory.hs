module KlisliCategory where

import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import UsefulFuncs as Us
import Numeric.Natural

type Weight = Integer

type Obj = Int
-- type Counter = Natural

data Codom 
    = OEnd Obj
    | WeightedOEnd Weight Obj
    | EveWin
    | AdamWin 
    deriving (Eq, Show)

type Mor = (Obj, Obj, [Codom])

-- todo: fix the case for EveWin, although it does not cause any errors. 
isSmallerOrEqualCodom :: Codom -> Codom -> Bool
isSmallerOrEqualCodom c1 c2 = case c1 of 
                                   OEnd j1 -> case c2 of 
                                                   OEnd j2 -> j1 == j2
                                                   _       -> False
                                   WeightedOEnd w1 j1 -> case c2 of 
                                                              WeightedOEnd w2 j2 -> w1 >= w2 && j1 == j2
                                                              AdamWin -> True
                                                              _ -> False
                                   AdamWin -> case c2 of 
                                                   AdamWin -> True 
                                                   _      -> False 
                                   EveWin  -> case c2 of 
                                                   OEnd j2 -> False
                                                   _       -> True


-- todo: fix the case for EveWin, although it does not cause any errors. 
isSmallerCodom :: Codom -> Codom -> Bool
isSmallerCodom c1 c2 = case c1 of 
                            OEnd j1 -> False
                            WeightedOEnd w1 j1 -> case c2 of 
                                                       WeightedOEnd w2 j2 -> w1 > w2 && j1 == j2
                                                       AdamWin -> True
                                                       _ -> False
                            AdamWin -> False
                            EveWin  -> case c2 of 
                                            OEnd j2 -> False
                                            EveWin -> False
                                            _       -> True




isSmallOrEqualMor :: Mor -> Mor -> Bool
isSmallOrEqualMor (m1, n1, f) (m2, n2, g) = (m1 == m2) && (n1 == n2) && (and [ isSmallerOrEqualCodom (f !! i) (g !! i)| i <- [1..m1] ]) 

isSmallerMor :: Mor -> Mor -> Bool
isSmallerMor (m1, n1, f) (m2, n2, g) = (m1 == m2) && (n1 == n2) && (and [ isSmallerOrEqualCodom (f !! i) (g !! i) | i <- [1..m1] ]) && (elem True [ isSmallerCodom (f !! i) (g !! i) | i <- [1..m1] ])


body :: Mor -> [Codom]
body (m, n, f) = f

isEqMor :: Mor -> Mor -> Bool
-- isEqMor f g = Us.fst f == Us.fst g && Us.snd f == Us.snd g && not (elem False [ body f i == body g i| i <- [1..Us.fst f]])
isEqMor f g = f == g


id :: Obj -> Mor
id m = (m, m, [OEnd i | i <- [0..m]])

sym :: Obj -> Obj -> Mor 
sym m n = (m + n, n + m, [OEnd 0] ++ [ OEnd (i + n)| i <- [1..m] ] ++ [OEnd (i) | i <- [1..n]]) 

-- comp does not check whether f and g is composable.
scomp :: Mor -> Mor -> Mor 
scomp f g = (Us.fst f, Us.snd g, scompC (body f) (body g))

scompBody :: Codom -> [Codom] -> Codom
scompBody y gb = case y of 
                      AdamWin -> AdamWin
                      EveWin -> EveWin
                      OEnd j -> gb !! j
                      WeightedOEnd p j -> case gb !! j of 
                                               OEnd k -> WeightedOEnd p k 
                                               WeightedOEnd q k -> WeightedOEnd (p+q) k 
                                               otherwise -> gb !! j

-- sequential composition 
scompC :: [Codom] -> [Codom] -> [Codom]
scompC fb gb = [ scompBody y gb | y <- fb ]
                       

pcomp :: Mor -> Mor -> Mor 
pcomp f g = (Us.fst f + Us.fst g, Us.snd f + Us.snd g, (body f) ++ map (reInd (Us.snd f)) (tail (body g)))

-- -- reindexing for parallel composition
reInd :: Obj -> Codom -> Codom
reInd x b = case b of 
              OEnd j -> OEnd (j + x)
              WeightedOEnd p j -> WeightedOEnd p (j + x)
              otherwise -> b

-- -- parallel compositon
-- pcompC :: Mor -> Mor -> Obj -> Codom
-- pcompC f g = \i -> case i <= Us.fst f of 
--                      True  -> body f $ i 
--                      False -> reInd (Us.snd f) (body g $ (i - (Us.fst f)))

-- trace operator
trace :: (Obj, Obj, Obj) -> Mor -> Mor 
trace (l, m, n) f = (m, n, traceC (l, m, n) f)

-- step1: checking whether the trace of f from (i + l) is looping or not:
-- step2-1 (yes case): call the denotation function for looping cases
-- step2-2 (no case): call the denotation function for not looping cases  
traceC ::(Obj, Obj, Obj) -> Mor -> [Codom]
traceC (l, m, n) f = [OEnd 0] ++ [traceBody (l, m, n) f (i+l)| i <- [1..m]]

traceBody :: (Obj, Obj, Obj) -> Mor -> Obj -> Codom
traceBody (l, m, n) f i = case isLooping fb i l [] of 
                               True -> denotationForInfiniteSemanticRun fb i []
                               False -> denotationForFiniteSemanticRun fb i l True 0 
                               where fb = body f

isLooping :: [Codom] -> Obj -> Obj -> [Obj] -> Bool
isLooping fb i l alreadyReached = case fb !! i of 
                                       OEnd j -> case elem j alreadyReached of 
                                                      True -> True
                                                      False -> case j > l of 
                                                                    True -> False 
                                                                    False -> isLooping fb j l (j:alreadyReached)
                                       WeightedOEnd p j -> case elem j alreadyReached of 
                                                                True -> True
                                                                False -> case j > l of 
                                                                              True -> False 
                                                                              False -> isLooping fb j l (j:alreadyReached)
                                       otherwise -> False
                        
-- the argument b is used for checking whether "WeightedOEnd" appers in loop.                       
denotationForFiniteSemanticRun :: [Codom] -> Obj -> Obj -> Bool -> Weight -> Codom
denotationForFiniteSemanticRun fb i l b w = case fb !! i of 
                                                 OEnd j -> case j > l of 
                                                                True -> case b of 
                                                                             True -> OEnd (j - l)
                                                                             False -> WeightedOEnd w (j - l)
                                                                False -> denotationForFiniteSemanticRun fb j l b w
                                                 WeightedOEnd p j -> case j > l of 
                                                                          True -> WeightedOEnd (w+p) (j - l)
                                                                          False -> denotationForFiniteSemanticRun fb j l False (w+p)
                                                 otherwise -> fb !! i

-- Never reach 'otherwise' because it is a denotaton for infinite semantic run.
denotationForInfiniteSemanticRun :: [Codom] -> Obj -> [Obj] ->Codom
denotationForInfiniteSemanticRun fb i alreadyReached = case fb !! i of 
                                                            OEnd j -> case elem j alreadyReached of 
                                                                           True -> isSatisfiedAcceptedCondition fb j j 0
                                                                           False -> denotationForInfiniteSemanticRun fb j (j:alreadyReached)
                                                            WeightedOEnd p j -> case elem j alreadyReached of 
                                                                                     True -> isSatisfiedAcceptedCondition fb j j 0
                                                                                     False -> denotationForInfiniteSemanticRun fb j (j:alreadyReached)
                                                            otherwise -> EveWin


-- Never reach 'otherwise' because it is an accpted condition for infinite semantic run.
isSatisfiedAcceptedCondition :: [Codom] -> Obj -> Obj -> Weight -> Codom
isSatisfiedAcceptedCondition fb i j sw = case fb !! i of 
                                              OEnd k -> case k == j of 
                                                             True -> periodicityLemma sw
                                                             False -> isSatisfiedAcceptedCondition fb k j sw
                                              WeightedOEnd p k -> case k == j of 
                                                                       True -> periodicityLemma (sw+p)
                                                                       False -> isSatisfiedAcceptedCondition fb k j (sw+p)
                                              otherwise -> EveWin

-- for comparison with QDPM
-- periodicityLemma :: Weight -> Codom
-- periodicityLemma sw = case sw > 0 of 
--                            True -> EveWin
--                            False -> AdamWin


periodicityLemma :: Weight -> Codom
periodicityLemma sw = case sw >= 0 of 
                           True -> EveWin
                           False -> AdamWin

showCodom :: Codom -> String
showCodom v = case v of 
                   OEnd m -> "OEnd " ++ (show m)
                   WeightedOEnd w m -> "WeightedOEnd " ++ " " ++ (show w) ++ " " ++ (show m)
                   EveWin -> "Existential Win"
                   AdamWin -> "Universal Win"


printFunc :: Mor -> String
printFunc (m, n, f) = recPrintFunc m f ""

recPrintFunc :: Obj -> [Codom] -> String -> String 
recPrintFunc 0 f s = s 
recPrintFunc m f s = recPrintFunc (m-1) f ( show m ++ ", " ++ showCodom (f !! m) ++ "\n" ++ s)