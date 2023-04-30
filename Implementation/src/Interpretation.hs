module Interpretation where
import Text.Parsec
import SyntaxOfFreeProp as FP
import qualified KlisliCategory as KC
import qualified CompactClosedSemanticCategory as CC
import UsefulFuncs as Us
import Data.Set as S
import Debug.Trace
-- the interpretation functor from the free prop to the semantic category.
interpretation :: Expr -> CC.Mor
interpretation (SComp e1 e2) = CC.scomp (interpretation e1) (interpretation e2)
interpretation (MProd e1 e2) = CC.pcomp (interpretation e1) (interpretation e2)
interpretation (Id DRight) = CC.id (1, 0)
interpretation (Id DLeft) = CC.id (0, 1)
interpretation (Syn DRight DRight) = CC.sym (1, 0) (1, 0)
interpretation (Syn DRight DLeft) = CC.sym (1, 0) (0, 1)
interpretation (Syn DLeft DRight) = CC.sym (0, 1) (1, 0)
interpretation (Syn DLeft DLeft) = CC.sym (0, 1) (0, 1)
interpretation (Unit DRight) = ((0, 0), (1, 1), S.singleton $ S.singleton [KC.OEnd 0, KC.OEnd 1] )
interpretation (Unit DLeft) = ((0, 0), (1, 1),  S.singleton $ S.singleton [KC.OEnd 0, KC.OEnd 1] )
interpretation (Counit DRight) = ((1, 1), (0, 0), S.singleton $ S.singleton [KC.OEnd 0, KC.OEnd 1] )
interpretation (Counit DLeft) = ((1, 1), (0, 0), S.singleton $ S.singleton [KC.OEnd 0, KC.OEnd 1] )
interpretation (Node Eve w dom codom) = ((numRight dom, numLeft dom), (numRight codom, numLeft codom), interpreationOfEveNode ((numRight dom) + (numLeft codom)) ((numRight codom) + (numLeft dom)) w )
interpretation (Node Adam w dom codom) = ((numRight dom, numLeft dom), (numRight codom, numLeft codom), S.singleton $ interpreationOfAdamNode ((numRight dom) + (numLeft codom)) ((numRight codom) + (numLeft dom)) w )

numRight :: FP.Obj -> KC.Obj
numRight [] = 0
numRight (DRight:l) = 1 + (numRight l)
numRight (DLeft:l) = numRight l


numLeft :: FP.Obj -> KC.Obj
numLeft [] = 0
numLeft (DLeft:l) = 1 + (numLeft l)
numLeft (DRight:l) = numLeft l

-- the value KC.OEnd 0 is dummy. If m == 0, then the morphism should be a sinleton set of initial function.
interpreationOfEveNode :: KC.Obj -> KC.Obj -> KC.Weight -> Set (Set [KC.Codom])
interpreationOfEveNode m n w = case m of 
                                    0 -> S.singleton $ S.singleton $ [KC.OEnd 0]
                                    _ -> case n of 
                                              0 -> S.singleton $ S.singleton $ [KC.OEnd 0] ++ [KC.AdamWin | i <- [1..m]]
                                              _ -> S.fromAscList [ S.singleton $ [KC.OEnd 0] ++ [KC.WeightedOEnd w i | j <- [1..m]] | i <- [1..n] ]


-- the value KC.OEnd 0 is dummy. If m == 0, then the morphism should be a sinleton set of initial function.
interpreationOfAdamNode :: KC.Obj -> KC.Obj -> KC.Weight -> Set [KC.Codom]
interpreationOfAdamNode m n w = case m of 
                                     0 -> S.singleton [KC.OEnd 0]
                                     _ -> case n of 
                                               0 -> S.singleton $ [KC.OEnd 0] ++ [KC.EveWin | i <- [1..m]]
                                               _ -> S.fromAscList $  [[KC.OEnd 0] ++ [KC.WeightedOEnd w i | j <- [1..m]] | i <- [1..n] ]