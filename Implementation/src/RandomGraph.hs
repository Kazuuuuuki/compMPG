module RandomGraph where 
import qualified ParserOfFreeProp as PFP
import qualified SyntaxOfFreeProp as SFP
import System.Random
import Control.Monad
    
-- create graphs randomly here

data Op 
     = SComp
     | PComp
     deriving (Eq, Show)

convertOp :: Int -> Op
convertOp 1 = SComp
convertOp _ = PComp

buildRandomNode :: SFP.Obj -> SFP.Obj -> IO SFP.Expr
buildRandomNode dom codom = do case dom == codom of 
                                    True  -> return $ SFP.gId dom
                                    False -> do role <- getStdRandom $ randomR (0,1) :: IO Int
                                                weight <- getStdRandom $ randomR (-100000,100000) :: IO Integer
                                                return $ (SFP.Node (convertRole role) weight dom codom) 
                              
buildBaseGraphWithSym :: SFP.Direction -> SFP.Direction -> IO SFP.Expr
buildBaseGraphWithSym d1 d2 = do option <- getStdRandom $ randomR (1,2) :: IO Int
                                 case option of 
                                      1 -> return $ (SFP.Syn d1 d2)
                                      _ -> buildRandomNode [d1, d2] [d2, d1]

buildBaseGraphWithUnit :: SFP.Direction -> IO SFP.Expr
buildBaseGraphWithUnit d = do option <- getStdRandom $ randomR (1,2) :: IO Int
                              case option of 
                                   1 -> return $ (SFP.Unit d)
                                   _ -> buildRandomNode [] [d, SFP.opDirection d]

buildBaseGraphWithCounit :: SFP.Direction -> IO SFP.Expr
buildBaseGraphWithCounit d = do option <- getStdRandom $ randomR (1,2) :: IO Int
                                case option of 
                                     1 -> return $ (SFP.Counit d)
                                     _ -> buildRandomNode [SFP.opDirection d, d] []

isSymAllowed :: SFP.Obj -> SFP.Obj -> Bool
isSymAllowed dom codom = (dom == [SFP.DRight, SFP.DRight] && codom == [SFP.DRight, SFP.DRight])
                       || (dom == [SFP.DRight, SFP.DLeft] && codom == [SFP.DLeft, SFP.DRight])
                       || (dom == [SFP.DLeft, SFP.DRight] && codom == [SFP.DRight, SFP.DLeft])
                       || (dom == [SFP.DLeft, SFP.DLeft] && codom == [SFP.DLeft, SFP.DLeft])

isUnitAllowed :: SFP.Obj -> SFP.Obj -> Bool
isUnitAllowed dom codom = (dom == [] && codom == [SFP.DRight, SFP.DLeft])   
                        || (dom == [] && codom == [SFP.DLeft, SFP.DRight])     

isCounitAllowed :: SFP.Obj -> SFP.Obj -> Bool
isCounitAllowed dom codom = (codom == [] && dom == [SFP.DRight, SFP.DLeft])   
                        || (codom == [] && dom == [SFP.DLeft, SFP.DRight])                   

buildBaseGraph :: SFP.Obj -> SFP.Obj -> IO SFP.Expr
buildBaseGraph dom codom = case isSymAllowed dom codom of 
                                True -> case dom of
                                             [SFP.DRight, SFP.DRight] ->  buildBaseGraphWithSym SFP.DRight SFP.DRight 
                                             [SFP.DRight, SFP.DLeft]  ->  buildBaseGraphWithSym SFP.DRight SFP.DLeft
                                             [SFP.DLeft, SFP.DLeft]   ->  buildBaseGraphWithSym SFP.DLeft SFP.DLeft
                                             _                        ->  buildBaseGraphWithSym SFP.DLeft SFP.DRight
                                False -> case isUnitAllowed dom codom of
                                              True -> case codom of 
                                                           [SFP.DRight, SFP.DLeft] -> buildBaseGraphWithUnit SFP.DRight
                                                           _                       -> buildBaseGraphWithUnit SFP.DLeft
                                              False -> case isCounitAllowed dom codom of
                                                            True -> case dom of 
                                                                         [SFP.DRight, SFP.DLeft] -> buildBaseGraphWithCounit SFP.DLeft
                                                                         _                       -> buildBaseGraphWithCounit SFP.DRight  
                                                            False -> buildRandomNode dom codom 


buildSCompGraph :: Int ->  SFP.Obj -> SFP.Obj -> IO SFP.Expr
buildSCompGraph k dom codom = do size <- getStdRandom $ randomR (1,20) :: IO Int
                                 directs <- replicateM size $ (getStdRandom $ randomR (0,1) :: IO Int)
                                 let ob = convertObj directs
                                 m1 <- createRandomGraph (k-1) dom ob
                                 m2 <- createRandomGraph (k-1) ob codom
                                 return $ (SFP.SComp m1 m2)


buildPCompGraph :: Int ->  SFP.Obj -> SFP.Obj -> IO SFP.Expr
buildPCompGraph k dom codom = do l_dom <- getStdRandom $ randomR (0,(length dom)) :: IO Int
                                 l_codom <- getStdRandom $ randomR (0,(length codom)) :: IO Int
                                 let dom1 = take l_dom dom
                                 let dom2 = drop l_dom dom
                                 let codom1 = take l_codom codom
                                 let codom2 = drop l_codom codom
                                 m1 <- createRandomGraph (k-1) dom1 codom1
                                 m2 <- createRandomGraph (k-1) dom2 codom2
                                 return $ (SFP.MProd m1 m2)

                                 

buildComposedGraph :: Int -> Op -> SFP.Obj -> SFP.Obj -> IO SFP.Expr
buildComposedGraph k op dom codom = case op of 
                                         SComp -> buildSCompGraph k dom codom
                                         PComp -> buildPCompGraph k dom codom 


createRandomGraph :: Int -> SFP.Obj -> SFP.Obj -> IO SFP.Expr
createRandomGraph k dom codom = case k of 
                                     1 -> buildBaseGraph dom codom
                                     _ -> do op <- getStdRandom $ randomR (1,2) :: IO Int
                                             buildComposedGraph k (convertOp op) dom codom 
                                
                

-- size is a depth of free prop
randomGraph :: Int -> IO SFP.Expr
randomGraph m = do size <- getStdRandom $ randomR (1,m) :: IO Int
                   let size_dom = [SFP.DRight, SFP.DLeft]
                --  let size_dom = [SFP.DRight]
               --   direction_dom <- replicateM size_dom $ (getStdRandom $ randomR (0,1) :: IO Int)
                   let size_codom = [SFP.DRight, SFP.DLeft]
                --  let size_codom = [SFP.DRight] 
               --   direction_codom <- replicateM size_codom $ (getStdRandom $ randomR (0,1) :: IO Int)
               --   createRandomGraph size (convertObj direction_dom) (convertObj direction_codom)
                   createRandomGraph size size_dom size_codom 
                 

convertRole :: Int -> SFP.Role
convertRole r = case r of 
                     1 -> SFP.Eve
                     _ -> SFP.Adam 

convertObj :: [Int] -> SFP.Obj
convertObj [] = []
convertObj (x:xs) = case x of 
                         1 -> SFP.DRight:(convertObj xs)
                         _ -> SFP.DLeft:(convertObj xs)


intToDouble :: Int -> Double
intToDouble k = read ((show k) ++ "." ++ "0") :: Double

-- randomNode :: IO()
-- randomNode = do
--     role <- getStdRandom $ randomR (0,1) :: IO Int
--     weight <- getStdRandom $ randomR (-20,20) :: IO Int
--     size_dom <- getStdRandom $ randomR (1,10) :: IO Int
--     direction_dom <- replicateM size_dom $ (getStdRandom $ randomR (0,1) :: IO Int)
--     size_codom <- getStdRandom $ randomR (1,10) :: IO Int
--     direction_codom <- replicateM size_codom $ (getStdRandom $ randomR (0,1) :: IO Int)
--     let node = buildNode role weight direction_dom direction_codom
--     print node
    