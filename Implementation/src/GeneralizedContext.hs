module GeneralizedContext where 
import qualified ParserOfFreeProp as PFP
import qualified SyntaxOfFreeProp as SFP
import CompactClosedSemanticCategory as CC
import qualified RandomGraph as RG
import ParserOfGame as PG
import System.Random
import Control.Monad
import System.IO
import System.TimeIt
import qualified Interpretation as Inter
import Realization as R
import OpenGame as OG
import Criterion.Main
import Control.Concurrent
import Debug.Trace
import qualified ToGraph as TG
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as LI
import Data.Monoid
import qualified RandomGraph as RG

type Var = Int
type Size = Int

data Judgement 
    = Var Var         
    | SComp Judgement Judgement 
    | PComp Judgement Judgement 
    | RTrace Judgement SFP.Obj SFP.Obj
    | LTrace Judgement SFP.Obj SFP.Obj
    deriving (Eq, Show, Read)

type Context = [CC.Mor]

pow :: Judgement -> Int -> Judgement 
pow j 1 = j 
pow j n = SComp ( pow j (n-1) ) j 

powOtimes :: Judgement -> Int -> Judgement
powOtimes j 1 = j 
powOtimes j n = PComp ( powOtimes j (n-1) ) j

read0Context :: [SFP.Expr] -> Context 
read0Context = map Inter.interpretation 

solveJudgement :: Context -> Judgement -> CC.Mor 
solveJudgement l (Var i)             = l !! i
solveJudgement l (SComp j1 j2)       = CC.scomp (solveJudgement l j1) (solveJudgement l j2)
solveJudgement l (PComp j1 j2)       = CC.pcomp (solveJudgement l j1) (solveJudgement l j2)
solveJudgement l (RTrace j1 dom cod) = CC.rtrace (solveJudgement l j1) (Inter.numRight dom, Inter.numLeft dom) (Inter.numRight cod, Inter.numLeft cod) 
solveJudgement l (LTrace j1 dom cod) = CC.ltrace (solveJudgement l j1) (Inter.numRight dom, Inter.numLeft dom) (Inter.numRight cod, Inter.numLeft cod) 

solveContext :: Int -> [String] -> Context -> Context 
solveContext 0 l c = c 
solveContext n l c = solveContext (n-1) (drop m (tail l)) (map (solveJudgement c) l2)
                     where m  = read $ head l :: Int 
                           l1 = take m (tail l)
                           l2 = map (read :: String -> Judgement) l1  

pTrace :: Show a => a -> a
pTrace b = trace (show b) b 


solveTest :: Int -> IO()
solveTest n = do let path = "generalized_contexts/" ++ "input" ++ (show n) ++ ".txt"
                 handle <- openFile path ReadMode 
                 contents <- hGetContents handle
                 let l = lines contents 
                 let n = pTrace (read $ head l :: Int)
                 let n0 = pTrace (read $ head $ tail l :: Int) 
            --      let n  = read $ head l :: Int
            --      let n0 = read $ head $ tail l :: Int
                 let l0 = take n0 (tail $ tail l)
                --  putStrLn $ show $ map (read :: String -> SFP.Expr) l0
                 let c0 = read0Context $ map (read :: String -> SFP.Expr) l0
            --      putStrLn $ CC.printMor $ head c0
                 let l1 = drop n0 (tail $ tail l)
                 let al = solveContext n l1 c0
                 let f  = head al 
                 putStrLn $ CC.printMor f
            --      let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
            --      let f2 = CC.pcomp f (CC.id (1, 0))
            --      let f3 = CC.pcomp (CC.id (1, 0)) (CC.counitR)
            --      let g  = CC.scomp (CC.scomp f1 f2) f3
            --      let g1 = CC.scomp CC.trivial g
            --      let g2 = CC.pcomp g1 (CC.id (0, 1))
            --      let g3 = CC.counitR
            --      let a  = CC.scomp (CC.scomp f1 g2) g3
            --      putStrLn $ CC.printMor a


-- solveBenchFile :: String -> IO()
-- solveBenchFile s = do handle <- openFile s ReadMode 
--                       contents <- hGetContents handle
--                       let l = lines contents 
--                       let n  = read $ head l :: Int
--                       let n0 = read $ head $ tail l :: Int
--                       let l0 = take n0 (tail $ tail l)
--                       let c0 = read0Context $ map (read :: String -> SFP.Expr) l0
--                       let l1 = drop n0 (tail $ tail l)
--                       let al = solveContext n l1 c0
--                       let f  = head al 
--                       let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
--                       let f2 = CC.pcomp f (CC.id (1, 0))
--                       let f3 = CC.pcomp (CC.id (1, 0)) (CC.counitR)
--                       let g  = CC.scomp (CC.scomp f1 f2) f3
--                       let g1 = CC.scomp CC.trivial g
--                       let g2 = CC.pcomp g1 (CC.id (0, 1))
--                       let g3 = CC.counitR
--                       let a  = CC.scomp (CC.scomp f1 g2) g3
--                       putStrLn $ CC.printMor a


solveInput :: Int -> IO()
solveInput n = do let path = "generalized_contexts/" ++ "input" ++ (show n) ++ ".txt"
                  handle <- openFile path ReadMode 
                  contents <- hGetContents handle
                  let l = lines contents 
                  let n  = read $ head l :: Int
                  let n0 = read $ head $ tail l :: Int
                  let l0 = take n0 (tail $ tail l)
                  let c0 = read0Context $ map (read :: String -> SFP.Expr) l0
                  let l1 = drop n0 (tail $ tail l)
                  let al = solveContext n l1 c0
                  let f  = head al 
                  putStrLn $ CC.printMor f



solveFile :: Int -> IO()
solveFile n = do let path = "generalized_contexts/" ++ "input" ++ (show n) ++ ".txt"
                 handle <- openFile path ReadMode 
                 contents <- hGetContents handle
                 let l = lines contents 
            --      let n = pTrace (read $ head l :: Int)
            --      let n0 = pTrace (read $ head $ tail l :: Int) 
                 let n  = read $ head l :: Int
                 let n0 = read $ head $ tail l :: Int
                 let l0 = take n0 (tail $ tail l)
                --  putStrLn $ show $ map (read :: String -> SFP.Expr) l0
                 let c0 = read0Context $ map (read :: String -> SFP.Expr) l0
            --      putStrLn $ CC.printMor $ head c0
                 let l1 = drop n0 (tail $ tail l)
                 let al = solveContext n l1 c0
                 let f  = head al 
            --      putStrLn $ CC.printMor f
                 let f1 = CC.pcomp (CC.id (1, 0)) (CC.unitR)
                 let f2 = CC.pcomp f (CC.id (1, 0))
                 let f3 = CC.pcomp (CC.id (1, 0)) (CC.counitR)
                 let g  = CC.scomp (CC.scomp f1 f2) f3
                 let g1 = CC.scomp CC.trivial g
                 let g2 = CC.pcomp g1 (CC.id (0, 1))
                 let g3 = CC.counitR
                 let a  = CC.scomp (CC.scomp f1 g2) g3
                 putStrLn $ CC.printMor a


translateJudgement :: [SFP.Expr] -> Judgement -> SFP.Expr
translateJudgement l (Var i)             = l !! i
translateJudgement l (SComp j1 j2)       = SFP.SComp (translateJudgement l j1) (translateJudgement l j2)
translateJudgement l (PComp j1 j2)       = SFP.MProd (translateJudgement l j1) (translateJudgement l j2)
translateJudgement l (RTrace j1 dom cod) = SFP.SComp (SFP.SComp f1 f2) f3
                                                          where f1 = SFP.MProd (SFP.Unit SFP.DLeft) (SFP.gId dom)
                                                                f2 = SFP.MProd (SFP.Id SFP.DLeft)  (translateJudgement l j1)
                                                                f3 = SFP.MProd (SFP.Counit SFP.DRight) (SFP.gId cod)
translateJudgement l (LTrace j1 dom cod) = SFP.SComp (SFP.SComp f1 f2) f3
                                                          where f1 = SFP.MProd (SFP.Unit SFP.DRight) (SFP.gId dom)
                                                                f2 = SFP.MProd (SFP.Id SFP.DRight)  (translateJudgement l j1)
                                                                f3 = SFP.MProd (SFP.Counit SFP.DLeft) (SFP.gId cod)

-- buildContexts1 :: Int -> Int -> Int -> [SFP.Expr] -> [SFP.Expr] -> String 
-- buildContexts1 0 sz csz dom cod = (show 1) ++ "\n" ++ (show $ RG.createRandomGraph sz dom cod)
-- buildContexts1 n sz csz dom cod = 
--                                   where ps = buildContexts1 (n-1) sz csz dom cod

-- -- arg1 num of files 
-- -- arg2 num of contexts 
-- -- arg3 size of variables 
-- createContexts1 :: Int -> Int -> Int -> IO()
-- createContexts1 i n sz = do let path = "generalized_contexts/" ++ "input" ++ (show i) ++ ".txt"
--                             h <- openFile path WriteMode
--                             let ans = show (n)
--                             let s   = buildContexts1 n sz csz [SFP.DRight, SFP.DLeft] [SFP.DRight, SFP.DLeft] 
--                             createContexts (i-1) n sz



translateContext :: Int -> [String] -> [SFP.Expr] -> [SFP.Expr] 
translateContext 0 l c = c
translateContext n l c = translateContext (n-1) (drop m (tail l)) (map (translateJudgement c) l2)
                         where m = read $ head l :: Int 
                               l1 = take m (tail l)
                               l2 = map (read :: String -> Judgement) l1  


-- translateFile :: String -> String -> IO()
-- translateFile s i = do let path = s
--                        handle <- openFile path ReadMode 
--                        contents <- hGetContents handle
--                        let l = lines contents 
--                        let n = pTrace (read $ head l :: Int)
--                        let n0 = pTrace (read $ head $ tail l :: Int) 
--                        let l0 = take n0 (tail $ tail l)
--                        let c0 = map (read :: String -> SFP.Expr) l0
--                        let l1 = drop n0 (tail $ tail l)
--                        let al = translateContext n l1 c0
--                        let f  = head al
--                        let f1 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Unit SFP.DLeft)
--                        let f2 = SFP.MProd f (SFP.Id SFP.DRight)
--                        let f3 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Counit SFP.DRight)
--                        let g  = SFP.SComp (SFP.SComp f1 f2) f3
--                        let g1 = SFP.SComp (SFP.Node SFP.Eve 0 [SFP.DRight, SFP.DRight] [SFP.DRight]) g
--                        let g2 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Unit SFP.DRight)
--                        let g3 = SFP.MProd (g1) (SFP.Id SFP.DLeft)
--                        let g4 = SFP.Counit SFP.DLeft
--                        let a  = SFP.SComp (SFP.SComp g2 g3) g4
--                        let tmp = TG.fromBTreeToGraph a
--                        let (num, es, rw) = tmp
--                        putStrLn $ show num
--                        let path2 = i
--                        h <- openFile path2 WriteMode
--                        LI.hPutStrLn h (L.pack ("parity " ++ show (num + 3) ++ ";"))
--                        LI.hPutStrLn h  (L.pack ("0" ++ " " ++ "0" ++ " " ++ (TG.fromRoleToInt SFP.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";"))
--                        TG.writeGraphInFile h 1 tmp
--                        LI.hPutStrLn h (L.pack ((show (num+1)) ++ " " ++ "-1" ++ " " ++ (TG.fromRoleToInt SFP.Eve) ++ " " ++ (show (num+1)) ++ " " ++ "\"DummyAdamWin"++ "\";"))
--                        LI.hPutStrLn h (L.pack ((show (num+2)) ++ " " ++ "1" ++ " " ++ (TG.fromRoleToInt SFP.Eve) ++ " " ++ (show (num+2)) ++ " " ++ "\"DummyEveWin"++ "\";"))
--                        hClose h


translateInput :: Int -> IO()
translateInput i = do let path = "generalized_contexts/" ++ "input" ++ (show i) ++ ".txt"
                      handle <- openFile path ReadMode 
                      contents <- hGetContents handle
                      let l = lines contents 
                      let n = pTrace (read $ head l :: Int)
                      let n0 = pTrace (read $ head $ tail l :: Int) 
                      let l0 = take n0 (tail $ tail l)
                      let c0 = map (read :: String -> SFP.Expr) l0
                      let l1 = drop n0 (tail $ tail l)
                      let al = translateContext n l1 c0
                      let f  = head al
                      let f1 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Unit SFP.DLeft)
                      let f2 = SFP.MProd f (SFP.Id SFP.DRight)
                      let f3 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Counit SFP.DRight)
                      let g  = SFP.SComp (SFP.SComp f1 f2) f3
                      let g1 = SFP.SComp (SFP.Node SFP.Eve 0 [SFP.DRight, SFP.DRight] [SFP.DRight]) g
                      let g2 = SFP.MProd (SFP.Id SFP.DRight) (SFP.Unit SFP.DRight)
                      let g3 = SFP.MProd (g1) (SFP.Id SFP.DLeft)
                      let g4 = SFP.Counit SFP.DLeft
                      let a  = SFP.SComp (SFP.SComp g2 g3) g4
                      let tmp = TG.fromBTreeToGraph a
                      let (num, es, rw) = tmp
                      putStrLn $ show num
                      let path2 = "pgs/input" ++ (show i) ++ ".txt"
                      h <- openFile path2 WriteMode
                      LI.hPutStrLn h (L.pack ("parity " ++ show (num + 3) ++ ";"))
                      LI.hPutStrLn h  (L.pack ("0" ++ " " ++ "0" ++ " " ++ (TG.fromRoleToInt SFP.Eve) ++ " " ++ "1" ++ " " ++ "\"En" ++ "1" ++ "\";"))
                      TG.writeGraphInFile h 1 tmp
                      LI.hPutStrLn h (L.pack ((show (num+1)) ++ " " ++ "-1" ++ " " ++ (TG.fromRoleToInt SFP.Eve) ++ " " ++ (show (num+1)) ++ " " ++ "\"DummyAdamWin"++ "\";"))
                      LI.hPutStrLn h (L.pack ((show (num+2)) ++ " " ++ "1" ++ " " ++ (TG.fromRoleToInt SFP.Eve) ++ " " ++ (show (num+2)) ++ " " ++ "\"DummyEveWin"++ "\";"))
                      hClose h


                     

        
