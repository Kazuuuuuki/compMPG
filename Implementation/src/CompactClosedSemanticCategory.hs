module CompactClosedSemanticCategory where


import Data.Set as S
import qualified Data.Map.Strict as M
import UsefulFuncs as Us
import qualified KlisliCategory as KC
import qualified TracedSemanticCategory as TS  


type Mor = ((KC.Obj, KC.Obj), (KC.Obj, KC.Obj),Set (Set [KC.Codom]))

body :: Mor -> Set (Set [KC.Codom])
body (_, _, s) = s

-- isEqMor :: Mor -> Mor -> Bool
-- isEqMor s t = Us.fst s == Us.fst t && Us.snd s == Us.snd t && (isEqMorBody (Us.fst s) (Us.snd s) (body s) (body t))

-- isEqMorBody :: (KC.Obj, KC.Obj) -> (KC.Obj, KC.Obj) ->  [[KC.Obj -> KC.Codom]] -> [[KC.Obj -> KC.Codom]] -> Bool
-- isEqMorBody (m_1, n_1) (m_2, n_2) bs bt = TS.isEqMorBody (m_1 + n_2) (m_2 + n_1) bs bt

id :: (KC.Obj, KC.Obj) -> Mor
id (m, n) = ((m, n), (m, n), TS.body (TS.id (m+n)))

-- -- used for creating a loop: sometimes we need to translate a morphism f:(1, 1) -> (1, 1) into f' : (1, 0) -> (0, 0).
trivialM :: [KC.Codom]
trivialM = [KC.OEnd 0, KC.WeightedOEnd 0 1, KC.WeightedOEnd 0 1]

trivial :: Mor 
trivial = ((2, 0), (1, 0), S.singleton $ S.singleton trivialM)

unitR :: Mor 
unitR = ((0, 0), (1, 1), TS.body (TS.id (1)))

counitR :: Mor
counitR = ((1, 1), (0, 0), TS.body (TS.id (1)))

sym :: (KC.Obj, KC.Obj)  -> (KC.Obj, KC.Obj) -> Mor
sym (m_1, n_1) (m_2, n_2) = ((m_1+m_2, n_2+n_1), (m_2+m_1, n_1+n_2),TS.body $ TS.pcomp (TS.sym m_1 m_2) (TS.sym n_2 n_1))

-- rm_duplicate ::  Mor -> Mor
-- rm_duplicate ((m1, n1), (m2, n2), s) = ((m1, n1), (m2, n2), TS.rmC_duplicate ((m1+n2), (m2+n1), s))

scomp :: Mor -> Mor -> Mor
scomp s t = (Us.fst s, Us.snd t, scompC s t)

scompC :: Mor -> Mor -> Set (Set [KC.Codom])
scompC s t = TS.body $ TS.trace (n_2, m_1+n_3, m_3+n_1) $ TS.scomp (TS.scomp (TS.scomp (TS.scomp f4 f3) f2) f1) f0
                where 
                f0 = TS.pcomp (TS.sym m_3 n_2) (TS.id n_1)
                f1 = TS.pcomp (m_2+n_3, m_3+n_2, bt) (TS.id n_1)
                f2 = TS.pcomp (TS.id m_2) (TS.sym n_1 n_3)
                f3 = TS.pcomp (m_1+n_2, m_2+n_1, bs) (TS.id n_3)
                f4 = TS.pcomp (TS.sym n_2 m_1) (TS.id n_3)
                (m_1, n_1) = Us.fst s
                (m_2, n_2) = Us.snd s
                (m_3, n_3) = Us.snd t
                bs = body s
                bt = body t

pcomp :: Mor -> Mor -> Mor
pcomp ((m_1, n_1), (m_2, n_2), s) ((m_3, n_3), (m_4, n_4), t) = ((m_1+m_3, n_3+n_1), (m_2+m_4, n_4+n_2), pcompC ((m_1, n_1), (m_2, n_2), s) ((m_3, n_3), (m_4, n_4), t)) 

pcompC :: Mor -> Mor -> Set (Set [KC.Codom])
pcompC s t =  TS.body $ TS.scomp (TS.scomp (TS.scomp f3 f2) f1) f0
                where 
                f0 = TS.pcomp (TS.id m_2) (TS.pcomp (m_3+n_4, m_4+n_3, bt) (TS.id n_1))
                f1 = TS.pcomp (TS.sym m_3 m_2) (TS.sym n_1 n_4)
                f2 = TS.pcomp (TS.id m_3) (TS.pcomp (m_1+n_2, m_2+n_1, bs) (TS.id n_4))
                f3 = TS.pcomp (TS.sym m_1 m_3) (TS.sym n_4 n_2)
                (m_1, n_1) = Us.fst s
                (m_2, n_2) = Us.snd s
                (m_3, n_3) = Us.fst t
                (m_4, n_4) = Us.snd t
                bs = body s
                bt = body t

rtrace :: Mor -> (KC.Obj, KC.Obj) -> (KC.Obj, KC.Obj) -> Mor 
rtrace f (m1, n1) (m2, n2) = scomp (scomp f1 f2) f3
                            where f1 = pcomp unitR (CompactClosedSemanticCategory.id (m1, n1))
                                  f2 = pcomp (CompactClosedSemanticCategory.id (0, 1)) f
                                  f3 = pcomp counitR (CompactClosedSemanticCategory.id (m2, n2))


ltrace :: Mor -> (KC.Obj, KC.Obj) -> (KC.Obj, KC.Obj) -> Mor 
ltrace f (m1, n1) (m2, n2) = scomp (scomp f1 f2) f3
                            where f1 = pcomp unitR (CompactClosedSemanticCategory.id (m1, n1))
                                  f2 = pcomp (CompactClosedSemanticCategory.id (1, 0)) f
                                  f3 = pcomp counitR (CompactClosedSemanticCategory.id (m2, n2))


printMor :: Mor -> String
printMor ((m1, n1), (m2, n2), s)  = "(" ++ (show m1) ++ "," ++ (show n1) ++ ")" ++ " -> " ++ "(" ++ (show m2) ++ "," ++ (show n2) ++ ")" ++ "\n"
                                        ++ TS.printMor ((m1+n2), (m2+n1), s) 

printMorEither :: Either String Mor -> String
printMorEither m = case m of 
                       Right m -> printMor m
                       Left s -> s

-- solveMor :: Mor -> KC.Obj -> String 
-- solveMor ((m1, n1), (m2, n2), s) i = TS.solveMor (m1 + n2, m2 + n1, s) i