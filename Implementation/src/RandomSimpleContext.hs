module RandomSimpleContext where 
import qualified ParserOfFreeProp as PFP
import qualified SyntaxOfFreeProp as SFP
import CompactClosedSemanticCategory as CC
import RandomGraph as RG
import ParserOfGame as PG
import System.Random
import Control.Monad
import Interpretation as Inter

-- creating contexts randomly here 


-- the number of operators
type Size = Int
-- the upper bound of number of variables
type Var = Int 
type OVar = [Int]
type Context = (Size, RG.Op, Var, OVar, [(Int, SFP.Expr)])
-- evaluated context
-- type EContext = (Size, RG.Op, Var, OVar, [(Int, CC.Mor)])

-- assign a game for each variable ramdomly
getVals :: Var -> Size -> IO [(Int, SFP.Expr)]
getVals 0 _ = pure []
getVals n m = do g <- RG.randomGraph m
                 l <- getVals (n-1) m
                 pure (l ++ [(n, g)])

fromContextToExpr :: Context -> Maybe SFP.Expr
fromContextToExpr (s, _, v, [], l)  = Nothing
fromContextToExpr (s, _, v, [i], l) = case evalExpr i l of 
                                           Right e -> Just e
                                           Left s  -> Nothing
fromContextToExpr (s, RG.SComp, v, i:xs, l) =  case evalExpr i l of 
                                                    Right e -> case fromContextToExpr (s, RG.SComp, v, xs, l) of 
                                                                    Just ex -> Just (SFP.SComp e ex)
                                                                    Nothing -> Nothing
                                                    Left s  -> Nothing
fromContextToExpr (s, RG.PComp, v, i:xs, l) = case evalExpr i l of 
                                                   Right e -> case fromContextToExpr (s, RG.PComp, v, xs, l) of 
                                                                   Just ex -> Just (SFP.MProd e ex)
                                                                   Nothing -> Nothing
                                                   Left s  -> Nothing


evalExpr :: Var -> [(Int, SFP.Expr)] -> Either String SFP.Expr
evalExpr v [] = Left ("value: " ++ show v ++ " is not defined.")
evalExpr v ((v1, s):xs) = case v1 == v of 
                               True  -> Right s 
                               False -> evalExpr v xs

createRandomContext :: Size -> Size -> RG.Op -> Var -> IO Context
createRandomContext s l op n = do ovar <- replicateM s $ (getStdRandom $ randomR (1,n) :: IO Int)
                                  vals <- getVals n l
                                  pure (s, op, n, ovar, vals)

-- the content of files
writeOvar :: OVar -> String 
writeOvar [] = ""
writeOvar (x:xs) = "v_{" ++ (show x) ++ "}" ++ "\n" ++ (writeOvar xs)

writeVals :: [(Int, SFP.Expr)] -> String 
writeVals [] = ""
writeVals ((x, e):xs) = "v_{" ++ (show x) ++ "}:" ++ (SFP.toString e) ++ "\n" ++ (writeVals xs)

writeContext :: Context -> String 
writeContext (size, op, var, ovar, l) = (show size) ++ "\n"
                                        ++ (show op) ++ "\n"
                                        ++ (writeOvar ovar) 
                                        ++ (writeVals l)

-- for EContext
-- evalContext :: Context -> EContext 
-- evalContext (s, o, v, ovar, l) = case l of 
--                                       []         -> (s, o, v, ovar, [])
--                                       ((x,e):xs) -> (s, o, v, ovar, (x, Inter.interpretation e):ys)
--                                                      where (_, _, _, _, ys) = evalContext (s, o, v, ovar, xs)

-- eval :: Var -> [(Int, CC.Mor)] -> Either String CC.Mor
-- eval v [] = Left ("value: " ++ show v ++ " is not defined.")
-- eval v ((v1, s):xs) = case v1 == v of 
--                            True  -> Right s 
--                            False -> eval v xs

-- parseSComp :: OVar -> [(Int, CC.Mor)] -> Either String CC.Mor 
-- parseSComp []  l = Left "scomp is failed"
-- parseSComp [i] l = eval i l
-- parseSComp (i:xs) l = case eval i l of 
--                            Right f -> case parseSComp xs l of 
--                                            Right g -> Right (CC.scomp f g)
--                                            Left  t -> Left t
--                            Left s  -> Left s

-- parsePComp :: OVar -> [(Int, CC.Mor)] -> Either String CC.Mor 
-- parsePComp []  l = Left "pcomp is failed"
-- parsePComp [i] l = eval i l
-- parsePComp (i:xs) l = case eval i l of 
--                            Right f -> case parsePComp xs l of 
--                                            Right g -> Right (CC.pcomp f g)
--                                            Left  t -> Left t
--                            Left s  -> Left s

-- parseEContext :: EContext -> Either String CC.Mor 
-- parseEContext (size, op, _, ovar, l) = case op of 
--                                             RG.SComp -> parseSComp ovar l
--                                             RG.PComp -> parsePComp ovar l

createRandomContexts :: Int -> Size -> Size -> RG.Op -> Var -> IO ()
createRandomContexts 0 _ _ _  _  = do print "finish creating contexts.\n"
createRandomContexts n m l op v  = do let path = "contexts/" ++ "test" ++ (show n) ++ ".txt"
                                      c <- createRandomContext m l op v
                                      writeFile path (writeContext c)
                                      createRandomContexts (n-1) m l op v


