module SyntaxOfGraph where 
import qualified CompactClosedSemanticCategory as CC
import qualified Interpretation as I
import qualified SyntaxOfFreeProp as S
import Debug.Trace
type Var = String 

data Base
    = Var Var 
    | SComp Base Base 
    | PComp Base Base 
    deriving (Eq, Show)

data Smallgraph 
    = Base Base 
    | SCompgraph Smallgraph Smallgraph 
    | PCompgraph Smallgraph Smallgraph
    deriving (Eq, Show)

type Valuation = [(Var, CC.Mor)]

type Input = (Smallgraph, Valuation)

assignment :: [(Var, S.Expr)] -> Valuation   
assignment []   = []
assignment (x:xs) = (v, I.interpretation e):(assignment xs)
                     where v = fst x 
                           e = snd x 

-- TODO eval v [] should be failed. This code should be fixed.
eval :: Var -> Valuation -> Either String CC.Mor
eval v [] = Left ("value: " ++ show v ++ " is not defined.")
eval v ((v1, s):xs) = case v1 == v of 
                         True  -> Right s 
                         False -> eval v xs

evalExpr :: Var -> [(Var, S.Expr)] -> Either String S.Expr
evalExpr v [] = Left ("value: " ++ show v ++ " is not defined.")
evalExpr v ((v1, s):xs) = case v1 == v of 
                               True  -> Right s 
                               False -> evalExpr v xs

translation :: (Smallgraph, [(Var, S.Expr)]) -> Either String S.Expr 
translation (s, val) = case s of 
                            Base b -> case b of 
                                           Var v -> evalExpr v val
                                           SComp b1 b2 -> case translation (Base b1, val) of 
                                                               Right v1 -> case translation (Base b2, val) of 
                                                                                Right v2 -> Right (S.SComp v1 v2)
                                                                                Left s   -> Left s
                                                               Left s   -> Left s
                                           PComp b1 b2 -> case translation (Base b1, val) of 
                                                               Right v1 -> case translation (Base b2, val) of 
                                                                                Right v2 -> Right (S.MProd v1 v2)
                                                                                Left s   -> Left s
                                                               Left s   -> Left s
                            SCompgraph s1 s2 -> case translation (s1, val) of
                                                     Right v1 -> case translation (s2, val) of 
                                                                      Right v2 -> Right (S.SComp v1 v2)
                                                                      Left s   -> Left s
                                                     Left s -> Left s
                            PCompgraph s1 s2 -> case translation (s1, val) of
                                                     Right v1 -> case translation (s2, val) of 
                                                                      Right v2 -> Right (S.MProd v1 v2)
                                                                      Left s   -> Left s
                                                     Left s -> Left s

interpretation :: (Smallgraph, Valuation) -> Either String CC.Mor
interpretation (s, val) = case s of 
                               Base b -> case b of 
                                              Var v -> eval v val
                                              SComp b1 b2 -> case interpretation (Base b1, val) of 
                                                                  Right v1 -> case interpretation (Base b2, val) of 
                                                                                  Right v2 -> Right (CC.scomp v1 v2)
                                                                                  Left s -> Left s
                                                                  Left s -> Left s
                                              PComp b1 b2 -> case interpretation (Base b1, val) of 
                                                                  Right v1 -> case interpretation (Base b2, val) of 
                                                                                  Right v2 -> Right (CC.pcomp v1 v2)
                                                                                  Left s -> Left s
                                                                  Left s -> Left s
                               SCompgraph s1 s2 -> case interpretation (s1, val) of 
                                                        Right v1 -> case interpretation (s2, val) of 
                                                                         Right v2 -> Right (CC.scomp v1 v2) 
                                                                         Left s -> Left s 
                                                        Left s -> Left s
                               PCompgraph s1 s2 -> case interpretation (s1, val) of 
                                                        Right v1 -> case interpretation (s2, val) of 
                                                                         Right v2 -> Right (CC.pcomp v1 v2) 
                                                                         Left s -> Left s 
                                                        Left s -> Left s
                            
-- interpretation :: (Smallgraph, Valuation) -> Either String CC.Mor
-- interpretation (s, val) = case s of 
--                                Base b -> case b of 
--                                               Var v -> eval v val
--                                               SComp b1 b2 -> case interpretation (Base b1, val) of 
--                                                                   Right v1 -> case interpretation (Base b2, val) of 
--                                                                                   Right v2 -> trace ("calculating" ++ (show b2)) (Right (CC.scomp v1 v2))
--                                                                                   Left s -> Left s
--                                                                   Left s -> Left s
--                                               PComp b1 b2 -> case interpretation (Base b1, val) of 
--                                                                   Right v1 -> case interpretation (Base b2, val) of 
--                                                                                   Right v2 -> Right (CC.pcomp v1 v2)
--                                                                                   Left s -> Left s
--                                                                   Left s -> Left s
--                                SCompgraph s1 s2 -> case interpretation (s1, val) of 
--                                                         Right v1 -> case interpretation (s2, val) of 
--                                                                          Right v2 -> trace ("calculating" ++ (show s2)) (Right (CC.scomp v1 v2))
--                                                                          Left s -> Left s 
--                                                         Left s -> Left s
--                                PCompgraph s1 s2 -> case interpretation (s1, val) of 
--                                                         Right v1 -> case interpretation (s2, val) of 
--                                                                          Right v2 -> Right (CC.pcomp v1 v2) 
--                                                                          Left s -> Left s 
--                                                         Left s -> Left s
                                   
        
