module Context where 
import qualified ParserOfFreeProp as PFP
import qualified SyntaxOfFreeProp as SFP
import CompactClosedSemanticCategory as CC
import RandomGraph as RG
import ParserOfGame as PG
import System.Random
import Control.Monad
import System.IO
import System.TimeIt
import Interpretation as Inter
import Realization as R
import OpenGame as OG
import Criterion.Main
import Control.Concurrent

type Var = Int
type Size = Int

data Context
    = Var Var Size SFP.Obj SFP.Obj  
    | SComp Context Context 
    | PComp Context Context 
    | RTrace Context SFP.Obj SFP.Obj
    | LTrace Context SFP.Obj SFP.Obj
    deriving (Eq, Show, Read)

data StepContext 
     = BaseContext  
     | SSComp StepContext StepContext 
     | SPComp StepContext StepContext 
     | SRTrace StepContext SFP.Obj SFP.Obj
     | SLTrace StepContext SFP.Obj SFP.Obj 
     deriving (Eq, Show, Read)
-- creating contexts randomly here 


type SContext= (Context, [SFP.Expr])
type EContext = (Context, [CC.Mor])
type Step = (StepContext, CC.Mor)
type SStep = (StepContext, SFP.Expr)


createBCInd :: Context -> StepContext -> Context 
createBCInd c BaseContext           = c 
createBCInd c (SSComp sc1 sc2)      = Context.SComp (createBCInd c sc1) (createBCInd c sc2)
createBCInd c (SPComp sc1 sc2)      = Context.PComp (createBCInd c sc1) (createBCInd c sc2)
createBCInd c (SRTrace sc1 dom cod) = RTrace (createBCInd c sc1) dom cod 
createBCInd c (SLTrace sc1 dom cod) = LTrace (createBCInd c sc1) dom cod  


 
createBC :: Context -> Int -> (Int -> StepContext) -> Context 
createBC c 1 f = c
createBC c n f = createBC (createBCInd c (f n)) (n-1) f

createVar :: [(Size, SFP.Obj, SFP.Obj)] -> IO [SFP.Expr]
createVar l = do al <- mapM ( \(size, dom, codom) -> RG.createRandomGraph size dom codom ) l 
                 pure $ al

createBaseAndSteps :: Int -> Context -> String -> Int -> [(Size, SFP.Obj, SFP.Obj)] -> IO ()
createBaseAndSteps 0 _ _ _ _  = putStrLn $ "finished creating contexts!"
createBaseAndSteps n c sc m l = do let path = "contexts/input" ++ (show n) ++ ".txt"
                                   al <- createVar l
                                   writeFile path (writeBaseAndStep c sc m al)
                                   putStrLn $ "finish" ++ (show n)
                                   createBaseAndSteps (n-1) c sc m l 


-- second argument is a variety of varibles
-- createSContext :: Context -> [(Size, SFP.Obj, SFP.Obj)] -> IO SContext
-- createSContext c l = do a <- mapM ( \(size, dom, codom) -> RG.createRandomGraph size dom codom ) l 
--                         pure $ (c, a)

-- createSContexts :: Int -> Context -> [(Size, SFP.Obj, SFP.Obj)] -> IO ()
-- createSContexts 0 _ _ = putStrLn $ "finished creating contexts!"
-- createSContexts n c a = do let path = "contexts/input" ++ (show n) ++ ".txt"
--                            sc <- createSContext c a 
--                            writeFile path $ writeSContext sc
--                            putStrLn $ "finish" ++ (show n)
--                            createSContexts (n-1) c a

translateFiles :: Int -> IO ()
translateFiles 0 = putStrLn $ "finished creating mean payoff games!"
translateFiles n = do let path = "contexts/input" ++ (show n) ++ ".txt"
                      handle <- openFile path ReadMode 
                      contents <- hGetContents handle
                      let l = lines contents
                      let c = read $ head l :: Context 
                      let s = map read (tail l)
                      let e = translation (c, s)
                      let f1 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Unit SFP.DLeft)
                      let f2 = SFP.MProd e (SFP.Id SFP.DRight)
                      let f3 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Counit SFP.DRight)
                      let g  = SFP.SComp (SFP.SComp f1 f2) f3
                      let g1 = SFP.SComp (SFP.Node SFP.Eve 0 [SFP.DRight, SFP.DRight] [SFP.DRight]) g
                      let g2 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Unit SFP.DRight)
                      let g3 = SFP.MProd (g1) (SFP.Id SFP.DLeft)
                      let g4 = SFP.Counit SFP.DLeft
                      let a  = SFP.SComp (SFP.SComp g2 g3) g4
                      let mg = OG.toPG $ R.realization a
                      let path2 = "pgs/input" ++ (show n) ++ ".txt"
                      writeFile path2 mg
                      Context.translateFiles (n-1)

-- solve :: Int -> IO()
-- solve n = do let path = "contexts/" ++ "input" ++ (show n) ++ ".txt"
--              handle <- openFile path ReadMode 
--              contents <- hGetContents handle
--              let l = lines contents 
--              let c = read $ head l :: Context 
--              let s = map read (tail l)
--              let ec = toEContext (c, s)
--              let f = Context.interpretation ec 
--              let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
--              let f2 = CC.pcomp f (CC.id (1, 0))
--              let f3 = CC.pcomp (CC.id (1, 0)) (CC.counitR)
--              let g  = CC.scomp (CC.scomp f1 f2) f3
--              let g1 = CC.scomp CC.trivial g
--              let g2 = CC.pcomp g1 (CC.id (0, 1))
--              let g3 = CC.counitR
--              let a  = CC.scomp (CC.scomp f1 g2) g3
--              -- to here
--              putStrLn $ (CC.printMor a)



-- parseFiles :: Int -> IO()
-- parseFiles 0 = putStrLn "finish solving mean payoff games!"
-- parseFiles n = do let path = "contexts/" ++ "input" ++ (show n) ++ ".txt"
--                   handle <- openFile path ReadMode 
--                   contents <- hGetContents handle
--                   let l = lines contents 
--                   let c = read $ head l :: Context 
--                   let s = map read (tail l)
--                   let ec = toEContext (c, s)
--                   let f = Context.interpretation ec 
--                   let path2 = "answers/" ++ "answer" ++ (show n) ++ ".txt"
--                   let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
--                   let f2 = CC.pcomp f (CC.id (1, 0))
--                   let f3 = CC.pcomp (CC.id (1, 0)) (CC.counitR)
--                   let g  = CC.scomp (CC.scomp f1 f2) f3
--                   let g1 = CC.scomp CC.trivial g
--                   let g2 = CC.pcomp g1 (CC.id (0, 1))
--                   let g3 = CC.counitR
--                   let a  = CC.scomp (CC.scomp f1 g2) g3
--                   writeFile path2 ((CC.printMor f) ++ "\n" ++ (CC.printMor a))
--                   --      Right f -> do let g1 = CC.scomp CC.trivial f
--                   --                    let g2 = CC.pcomp g1 (CC.id (0, 1))
--                   --                    let g3 = CC.counitR
--                   --                    let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
--                   --                    let a  = CC.scomp (CC.scomp f1 g2) g3
--                   --                    writeFile path2 ((CC.printMor f) ++ "\n" ++ (CC.printMor a))
--                   putStrLn $ "finish" ++ (show n)         
--                   Context.parseFiles (n-1)  

-- algo :: SContext -> IO ()
-- algo (c, s) =      do let ec = toEContext (c, s)
--                       let f = Context.interpretation ec 
--                       let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
--                       let f2 = CC.pcomp f (CC.id (1, 0))
--                       let f3 = CC.pcomp (CC.id (1, 0)) (CC.counitR)
--                       let g  = CC.scomp (CC.scomp f1 f2) f3
--                       let g1 = CC.scomp CC.trivial g
--                       let g2 = CC.pcomp g1 (CC.id (0, 1))
--                       let g3 = CC.counitR
--                       let a  = CC.scomp (CC.scomp f1 g2) g3
--                       putStrLn $ CC.printMor a
--                     --   let test = testf 1
--                     --   putStrLn $ show test 
--                     --   threadDelay (2 * 1000 * 1000)
--                     --   putStrLn "finished!!!!"


-- algo2 :: SContext -> String 
-- algo2 (c, s) =  CC.printMor a
--                 where ec = toEContext (c, s)
--                       f = Context.interpretation ec 
--                       f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
--                       f2 = CC.pcomp f (CC.id (1, 0))
--                       f3 = CC.pcomp (CC.id (1, 0)) (CC.counitR)
--                       g  = CC.scomp (CC.scomp f1 f2) f3
--                       g1 = CC.scomp CC.trivial g
--                       g2 = CC.pcomp g1 (CC.id (0, 1))
--                       g3 = CC.counitR
--                       a  = CC.scomp (CC.scomp f1 g2) g3

-- algo3 :: Int -> Int 
-- algo3 n = algo3 n 

-- solveFile :: Int -> IO()
-- solveFile n = do let path = "contexts/" ++ "input" ++ (show n) ++ ".txt"
--                  handle <- openFile path ReadMode 
--                  contents <- hGetContents handle 
--                  let l = lines contents 

leftAndRight :: Int -> [SFP.Direction]
leftAndRight 1 = [SFP.DRight, SFP.DLeft]
leftAndRight n = (SFP.DRight):(SFP.DLeft:(leftAndRight (n-1)))

solveFile :: Int -> IO()
solveFile n = do let path = "contexts/" ++ "input" ++ (show n) ++ ".txt"
                 handle <- openFile path ReadMode 
                 contents <- hGetContents handle
                 let l = lines contents 
                 let c = read $ head l :: Context 
                 let sc = read $ (head $ tail l) :: String
                 let i = read $ (head $ tail $ tail l) :: Int
                 let v = map read (tail $ tail $ tail l) 
                --  let s = map read (tail l)
                 let ec = toEContext (c, v)
                 let pf = Context.interpretation ec 
                --  putStrLn $ CC.printMor pf
                 let f = case sc of 
                              "case1" -> powStep i sc5 pf
                              _       -> pf
                --  putStrLn $ CC.printMor f
                 let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
                 let f2 = CC.pcomp f (CC.id (1, 0))
                 let f3 = CC.pcomp (CC.id (1, 0)) (CC.counitR)
                 let g  = CC.scomp (CC.scomp f1 f2) f3
                 let g1 = CC.scomp CC.trivial g
                 let g2 = CC.pcomp g1 (CC.id (0, 1))
                 let g3 = CC.counitR
                 let a  = CC.scomp (CC.scomp f1 g2) g3
                 putStrLn $ CC.printMor a


                 
--                  -- to here 
--                 --  let path2 = "answers/" ++ "answer" ++ (show n) ++ ".txt"
--                 --  writeFile path2 ((CC.printMor f) ++ "\n" ++ (CC.printMor a))
--                   --      Right f -> do let g1 = CC.scomp CC.trivial f
--                   --                    let g2 = CC.pcomp g1 (CC.id (0, 1))
--                   --                    let g3 = CC.counitR
--                   --                    let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
--                   --                    let a  = CC.scomp (CC.scomp f1 g2) g3
--                   --                    writeFile path2 ((CC.printMor f) ++ "\n" ++ (CC.printMor a))
                --  putStrLn $ "finish" ++ (show n)      

-- testf :: Int -> Int 
-- testf n = testf n

-- fib :: Int -> Int 
-- fib m | m < 0     = -1
--       | otherwise = go m
--   where
--     go 0 = 0
--     go 1 = 1
--     go n = go (n-1) + go (n-2)


-- mParseFile :: Int -> IO()
-- mParseFile n = do let path = "contexts/" ++ "input" ++ (show n) ++ ".txt"
--                   handle <- openFile path ReadMode 
--                   contents <- hGetContents handle
--                   let l = lines contents 
--                   let c = read $ head l :: Context 
--                   let s = map read (tail l)
--                   putStrLn $ show c
--                   putStrLn $ show s 
--                   timeIt $ algo (c, s)
               
                --  defaultMain [ bgroup "solve" [bench "1" $ whnf algo2 (c, s)]]
                --  to here 
                --  let path2 = "answers/" ++ "answer" ++ (show n) ++ ".txt"
                --  writeFile path2 ((CC.printMor f) ++ "\n" ++ (CC.printMor a))
                --        Right f -> do let g1 = CC.scomp CC.trivial f
                --                      let g2 = CC.pcomp g1 (CC.id (0, 1))
                --                      let g3 = CC.counitR
                --                      let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
                --                      let a  = CC.scomp (CC.scomp f1 g2) g3
                --                      writeFile path2 ((CC.printMor f) ++ "\n" ++ (CC.printMor a))
                --  putStrLn $ "finish" ++ (show n)         

writeBaseAndStep :: Context -> String -> Int -> [SFP.Expr] -> String 
writeBaseAndStep c sc m l = (show c) ++ "\n" ++ (show sc) ++ "\n" ++ (show m) ++ "\n" ++ (writeAssignment l)


-- writeSContext :: SContext -> String 
-- writeSContext (c, l) = (show c) ++ "\n" ++ writeAssignment l

writeAssignment :: [SFP.Expr] -> String 
writeAssignment [] = ""
writeAssignment (x:xs) = (show x) ++ "\n" ++ (writeAssignment xs)

toEContext :: SContext -> EContext 
toEContext (c, l) = (c, map (\s -> Inter.interpretation s) l)

translation :: SContext -> SFP.Expr 
translation (c, l) = case c of 
                         Context.Var i _ _ _           -> l !! i
                         Context.SComp c1 c2           -> SFP.SComp (Context.translation (c1, l)) (Context.translation (c2, l))
                         Context.PComp c1 c2           -> SFP.MProd (Context.translation (c1, l)) (Context.translation (c2, l))
                         Context.RTrace c1 dom codom   -> SFP.SComp (SFP.SComp f1 f2) f3
                                                          where f1 = SFP.MProd (SFP.Unit SFP.DLeft) (SFP.gId dom)
                                                                f2 = SFP.MProd (SFP.Id SFP.DLeft)  (Context.translation (c1, l))
                                                                f3 = SFP.MProd (SFP.Counit SFP.DRight) (SFP.gId codom)
                         Context.LTrace c1 dom codom   -> SFP.SComp (SFP.SComp f1 f2) f3
                                                          where f1 = SFP.MProd (SFP.Unit SFP.DRight) (SFP.gId dom)
                                                                f2 = SFP.MProd (SFP.Id SFP.DRight)  (Context.translation (c1, l))
                                                                f3 = SFP.MProd (SFP.Counit SFP.DLeft) (SFP.gId codom)

stepTranslation :: SStep -> SFP.Expr
stepTranslation (c, l) = case c of 
                              Context.BaseContext          -> l 
                              Context.SSComp c1 c2         -> SFP.SComp  (Context.stepTranslation (c1, l)) (Context.stepTranslation (c2, l))
                              Context.SPComp c1 c2         -> SFP.MProd (Context.stepTranslation (c1, l)) (Context.stepTranslation (c2, l))
                              Context.SRTrace c1 dom codom -> SFP.SComp (SFP.SComp f1 f2) f3
                                                              where f1 = SFP.MProd (SFP.Unit SFP.DLeft) (SFP.gId dom)
                                                                    f2 = SFP.MProd (SFP.Id SFP.DLeft)  (Context.stepTranslation (c1, l))
                                                                    f3 = SFP.MProd (SFP.Counit SFP.DRight) (SFP.gId codom)
                              Context.SLTrace c1 dom codom -> SFP.SComp (SFP.SComp f1 f2) f3
                                                              where f1 = SFP.MProd (SFP.Unit SFP.DRight) (SFP.gId dom)
                                                                    f2 = SFP.MProd (SFP.Id SFP.DRight)  (Context.stepTranslation (c1, l))
                                                                    f3 = SFP.MProd (SFP.Counit SFP.DLeft) (SFP.gId codom)


interpretation :: EContext -> CC.Mor
interpretation (c, l) = case c of 
                             Context.Var i _ _ _           -> l !! i
                             Context.SComp c1 c2           -> CC.scomp (Context.interpretation (c1, l)) (Context.interpretation (c2, l))
                             Context.PComp c1 c2           -> CC.pcomp (Context.interpretation (c1, l)) (Context.interpretation (c2, l))
                             Context.RTrace c1 dom codom   -> CC.rtrace (Context.interpretation (c1, l)) (Inter.numRight dom, Inter.numLeft dom) (Inter.numRight codom, Inter.numLeft codom) 
                             Context.LTrace c1 dom codom   -> CC.ltrace (Context.interpretation (c1, l)) (Inter.numRight dom, Inter.numLeft dom) (Inter.numRight codom, Inter.numLeft codom) 

stepInterpretation :: Step -> CC.Mor 
stepInterpretation (sc, l) = case sc of 
                             Context.BaseContext            -> l
                             Context.SSComp c1 c2           -> CC.scomp (Context.stepInterpretation (c1, l)) (Context.stepInterpretation (c2, l))
                             Context.SPComp c1 c2           -> CC.pcomp (Context.stepInterpretation (c1, l)) (Context.stepInterpretation (c2, l))
                             Context.SRTrace c1 dom codom   -> CC.rtrace (Context.stepInterpretation (c1, l)) (Inter.numRight dom, Inter.numLeft dom) (Inter.numRight codom, Inter.numLeft codom) 
                             Context.SLTrace c1 dom codom   -> CC.ltrace (Context.stepInterpretation (c1, l)) (Inter.numRight dom, Inter.numLeft dom) (Inter.numRight codom, Inter.numLeft codom) 

powStep :: Int -> (Int -> StepContext) -> CC.Mor -> CC.Mor 
powStep 1 fs g = g
powStep n fs g = powStep (n-1) fs (stepInterpretation (fs n, g))

-------------sample contexts--------------------

sc5 :: Int -> StepContext 
sc5 n = SLTrace (SRTrace (powSC (BaseContext) 6) (SFP.DLeft:(leftAndRight (n-1))) (SFP.DLeft:(leftAndRight (n-1)))) (leftAndRight (n-1)) (leftAndRight (n-1))

pow :: Context -> Int -> Context 
pow c 1 = c 
pow c n = Context.SComp ( pow c (n-1) ) c 

powSC :: StepContext -> Int -> StepContext
powSC sc 1 = sc 
powSC sc n = SSComp ( powSC sc (n-1) ) sc 

gv1 :: Int -> Int -> Context 
gv1 n s = Var 0 s (leftAndRight n) (leftAndRight n)

gbc1 :: (Int, Int, Int) -> Context 
gbc1 (s, n, m) = pow (gv1 n s) m

gsc1 :: Int -> Int -> StepContext 
gsc1 l n = SLTrace (SRTrace (powSC (BaseContext) l) (SFP.DLeft:(leftAndRight (n-1))) (SFP.DLeft:(leftAndRight (n-1)))) (leftAndRight (n-1)) (leftAndRight (n-1)) 

gac1 :: (Int, Int) -> [(Int, SFP.Obj, SFP.Obj)]
gac1 (s, n) = [(s, leftAndRight n, leftAndRight n)]

-- assignment :: [(Var, S.Expr)] -> Valuation   
-- assignment []   = []
-- assignment (x:xs) = (v, I.interpretation e):(assignment xs)
--                      where v = fst x 
--                            e = snd x 

-- -- TODO eval v [] should be failed. This code should be fixed.
-- eval :: Var -> Valuation -> Either String CC.Mor
-- eval v [] = Left ("value: " ++ show v ++ " is not defined.")
-- eval v ((v1, s):xs) = case v1 == v of 
--                          True  -> Right s 
--                          False -> eval v xs

-- evalExpr :: Var -> [(Var, S.Expr)] -> Either String S.Expr
-- evalExpr v [] = Left ("value: " ++ show v ++ " is not defined.")
-- evalExpr v ((v1, s):xs) = case v1 == v of 
--                                True  -> Right s 
--                                False -> evalExpr v xs

-- translation :: (Smallgraph, [(Var, S.Expr)]) -> Either String S.Expr 
-- translation (s, val) = case s of 
--                             Base b -> case b of 
--                                            Var v -> evalExpr v val
--                                            SComp b1 b2 -> case translation (Base b1, val) of 
--                                                                Right v1 -> case translation (Base b2, val) of 
--                                                                                 Right v2 -> Right (S.SComp v1 v2)
--                                                                                 Left s   -> Left s
--                                                                Left s   -> Left s
--                                            PComp b1 b2 -> case translation (Base b1, val) of 
--                                                                Right v1 -> case translation (Base b2, val) of 
--                                                                                 Right v2 -> Right (S.MProd v1 v2)
--                                                                                 Left s   -> Left s
--                                                                Left s   -> Left s
--                             SCompgraph s1 s2 -> case translation (s1, val) of
--                                                      Right v1 -> case translation (s2, val) of 
--                                                                       Right v2 -> Right (S.SComp v1 v2)
--                                                                       Left s   -> Left s
--                                                      Left s -> Left s
--                             PCompgraph s1 s2 -> case translation (s1, val) of
--                                                      Right v1 -> case translation (s2, val) of 
--                                                                       Right v2 -> Right (S.MProd v1 v2)
--                                                                       Left s   -> Left s
--                                                      Left s -> Left s

-- interpretation :: (Smallgraph, Valuation) -> Either String CC.Mor
-- interpretation (s, val) = case s of 
--                                Base b -> case b of 
--                                               Var v -> eval v val
--                                               SComp b1 b2 -> case interpretation (Base b1, val) of 
--                                                                   Right v1 -> case interpretation (Base b2, val) of 
--                                                                                   Right v2 -> Right (CC.scomp v1 v2)
--                                                                                   Left s -> Left s
--                                                                   Left s -> Left s
--                                               PComp b1 b2 -> case interpretation (Base b1, val) of 
--                                                                   Right v1 -> case interpretation (Base b2, val) of 
--                                                                                   Right v2 -> Right (CC.pcomp v1 v2)
--                                                                                   Left s -> Left s
--                                                                   Left s -> Left s
--                                SCompgraph s1 s2 -> case interpretation (s1, val) of 
--                                                         Right v1 -> case interpretation (s2, val) of 
--                                                                          Right v2 -> Right (CC.scomp v1 v2) 
--                                                                          Left s -> Left s 
--                                                         Left s -> Left s
--                                PCompgraph s1 s2 -> case interpretation (s1, val) of 
--                                                         Right v1 -> case interpretation (s2, val) of 
--                                                                          Right v2 -> Right (CC.pcomp v1 v2) 
--                                                                          Left s -> Left s 
--                                                         Left s -> Left s


-- -- assign a game for each variable ramdomly
-- getVals :: Var -> Size -> IO [(Int, SFP.Expr)]
-- getVals 0 _ = pure []
-- getVals n m = do g <- RG.randomGraph m
--                  l <- getVals (n-1) m
--                  pure (l ++ [(n, g)])

-- fromContextToExpr :: Context -> Maybe SFP.Expr
-- fromContextToExpr (s, _, v, [], l)  = Nothing
-- fromContextToExpr (s, _, v, [i], l) = case evalExpr i l of 
--                                            Right e -> Just e
--                                            Left s  -> Nothing
-- fromContextToExpr (s, RG.SComp, v, i:xs, l) =  case evalExpr i l of 
--                                                     Right e -> case fromContextToExpr (s, RG.SComp, v, xs, l) of 
--                                                                     Just ex -> Just (SFP.SComp e ex)
--                                                                     Nothing -> Nothing
--                                                     Left s  -> Nothing
-- fromContextToExpr (s, RG.PComp, v, i:xs, l) = case evalExpr i l of 
--                                                    Right e -> case fromContextToExpr (s, RG.PComp, v, xs, l) of 
--                                                                    Just ex -> Just (SFP.MProd e ex)
--                                                                    Nothing -> Nothing
--                                                    Left s  -> Nothing


-- evalExpr :: Var -> [(Int, SFP.Expr)] -> Either String SFP.Expr
-- evalExpr v [] = Left ("value: " ++ show v ++ " is not defined.")
-- evalExpr v ((v1, s):xs) = case v1 == v of 
--                                True  -> Right s 
--                                False -> evalExpr v xs

-- createRandomContext :: Size -> Size -> RG.Op -> Var -> IO Context
-- createRandomContext s l op n = do ovar <- replicateM s $ (getStdRandom $ randomR (1,n) :: IO Int)
--                                   vals <- getVals n l
--                                   pure (s, op, n, ovar, vals)

-- -- the content of files
-- writeOvar :: OVar -> String 
-- writeOvar [] = ""
-- writeOvar (x:xs) = "v_{" ++ (show x) ++ "}" ++ "\n" ++ (writeOvar xs)

-- writeVals :: [(Int, SFP.Expr)] -> String 
-- writeVals [] = ""
-- writeVals ((x, e):xs) = "v_{" ++ (show x) ++ "}:" ++ (SFP.toString e) ++ "\n" ++ (writeVals xs)

-- writeContext :: Context -> String 
-- writeContext (size, op, var, ovar, l) = (show size) ++ "\n"
--                                         ++ (show op) ++ "\n"
--                                         ++ (writeOvar ovar) 
--                                         ++ (writeVals l)

-- -- for EContext
-- -- evalContext :: Context -> EContext 
-- -- evalContext (s, o, v, ovar, l) = case l of 
-- --                                       []         -> (s, o, v, ovar, [])
-- --                                       ((x,e):xs) -> (s, o, v, ovar, (x, Inter.interpretation e):ys)
-- --                                                      where (_, _, _, _, ys) = evalContext (s, o, v, ovar, xs)

-- -- eval :: Var -> [(Int, CC.Mor)] -> Either String CC.Mor
-- -- eval v [] = Left ("value: " ++ show v ++ " is not defined.")
-- -- eval v ((v1, s):xs) = case v1 == v of 
-- --                            True  -> Right s 
-- --                            False -> eval v xs

-- -- parseSComp :: OVar -> [(Int, CC.Mor)] -> Either String CC.Mor 
-- -- parseSComp []  l = Left "scomp is failed"
-- -- parseSComp [i] l = eval i l
-- -- parseSComp (i:xs) l = case eval i l of 
-- --                            Right f -> case parseSComp xs l of 
-- --                                            Right g -> Right (CC.scomp f g)
-- --                                            Left  t -> Left t
-- --                            Left s  -> Left s

-- -- parsePComp :: OVar -> [(Int, CC.Mor)] -> Either String CC.Mor 
-- -- parsePComp []  l = Left "pcomp is failed"
-- -- parsePComp [i] l = eval i l
-- -- parsePComp (i:xs) l = case eval i l of 
-- --                            Right f -> case parsePComp xs l of 
-- --                                            Right g -> Right (CC.pcomp f g)
-- --                                            Left  t -> Left t
-- --                            Left s  -> Left s

-- -- parseEContext :: EContext -> Either String CC.Mor 
-- -- parseEContext (size, op, _, ovar, l) = case op of 
-- --                                             RG.SComp -> parseSComp ovar l
-- --                                             RG.PComp -> parsePComp ovar l

-- createRandomContexts :: Int -> Size -> Size -> RG.Op -> Var -> IO ()
-- createRandomContexts 0 _ _ _  _  = do print "finish creating contexts.\n"
-- createRandomContexts n m l op v  = do let path = "contexts/" ++ "test" ++ (show n) ++ ".txt"
--                                       c <- createRandomContext m l op v
--                                       writeFile path (writeContext c)
--                                       createRandomContexts (n-1) m l op v