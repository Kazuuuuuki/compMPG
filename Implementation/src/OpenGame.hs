module OpenGame where

import Text.Parsec
import SyntaxOfFreeProp as FP
import UsefulFuncs as Us
import Data.Set as Set
import qualified RightwardOpenGame as R
import qualified Data.Vector as V

-- we define a category of open game here


type Mor = ((R.Obj, R.Obj), (R.Obj, R.Obj), R.ROG)

id :: (R.Obj, R.Obj) -> Mor
id (m, n) = ((m, n), (m, n), R.id (m+n))

sym :: (R.Obj, R.Obj)  -> (R.Obj, R.Obj) -> Mor
sym (m_1, n_1) (m_2, n_2) = ((m_1+m_2, n_2+n_1), (m_2+m_1, n_1+n_2), R.pcomp (R.sym m_1 m_2) (R.sym n_2 n_1))

scomp :: Mor -> Mor -> Mor 
scomp f g = ((m_1, n_1), (m_3, n_3), R.trace n_2 (m_1+n_3) (m_3+n_1) $ R.scomp (R.scomp (R.scomp (R.scomp f4 f3) f2) f1) f0)
            where  
            f0 = R.pcomp (R.sym m_3 n_2) (R.id n_1)
            f1 = R.pcomp bg (R.id n_1)
            f2 = R.pcomp (R.id m_2) (R.sym n_1 n_3)
            f3 = R.pcomp bf (R.id n_3)
            f4 = R.pcomp (R.sym n_2 m_1) (R.id n_3)
            ((m_1, n_1), (m_2, n_2), bf) = f
            ((_, _), (m_3, n_3), bg) = g

pcomp :: Mor -> Mor -> Mor 
pcomp f g = ((m_1+m_3, n_3+n_1), (m_2+m_4, n_4+n_2), R.scomp (R.scomp (R.scomp f3 f2) f1) f0)
            where 
            f0 = R.pcomp (R.id m_2) (R.pcomp bg (R.id n_1))
            f1 = R.pcomp (R.sym m_3 m_2) (R.sym n_1 n_4)
            f2 = R.pcomp (R.id m_3) (R.pcomp bf (R.id n_4))
            f3 = R.pcomp (R.sym m_1 m_3) (R.sym n_4 n_2)
            ((m_1, n_1), (m_2, n_2), bf) = f
            ((m_3, n_3), (m_4, n_4), bg) = g

-- toPG is used in the tranlation from contexts to parity games in the style of PGsolver.
-- In PGsolver, they assume that every node in parity games has a successor, although we do not assume in our setting.
-- Therefore, we add two dummy nodes which are self-looping.
toPG :: Mor -> String 
toPG ((m1, n1), (m2, n2), (m3, n3, p, e, r, w)) = "parity " ++ show ( m1 + n1 + m2 + n2 + p + 2) ++ ";"++ "\n" 
                                                ++ descriptNode 0 (m1 + n2, m2 + n1, p, e, r, w) 
                                                ++ show (m3 + n3 + p) ++ " " ++ (show (-1)) ++ " " ++ (fromRoleToInt (R.Eve)) ++ " " ++ show (m3 + n3 + p) ++ " " ++ "\"DummyAdamWin\"" ++ ";"++ "\n"
                                                ++ show (m3 + n3 + p+1) ++ " " ++ (show (1)) ++ " " ++ (fromRoleToInt (R.Eve)) ++ " " ++ show (m3 + n3 + p+1) ++ " " ++ "\"DummyEveWin\""++ ";"


-- fromNodeToInt :: R.Obj -> R.Obj -> R.Node -> String
-- fromNodeToInt m p (R.En i)    = show (i - 1)
-- fromNodeToInt m p (R.INode i) = show (i + m - 1)
-- fromNodeToInt m p (R.Ex i)    = show (i + m + p - 1)

-- succersors :: Int -> R.ROG -> String 
-- succersors i (m, n, p, e, r, w) = case i < m of 
--                                        True  -> Us.join "," [ fromNodeToInt m p k2 | (k1, k2) <- le, k1 == (R.En (i+1))]
--                                        False -> case i < m + p of 
--                                                      True  -> Us.join "," [ fromNodeToInt m p k2  | (k1, k2) <- le, k1 == (R.INode (i-m+1))]
--                                                      False -> ""                                    
--                                   where le = Set.toList e


succersors :: Int -> R.ROG -> String 
succersors i (m, n, p, e, r, w) = case (e V.! i) == V.empty of 
                                       True -> case r V.! (i-m) of 
                                                  R.Eve -> show (m+n+p)
                                                  R.Adam -> show (m+n+p+1)
                                       False   -> tail $ V.foldl (\s -> \i -> s ++ "," ++ show i) "" l
                                                  where l = e V.! i
 
fromRoleToInt :: R.Role -> String 
fromRoleToInt R.Eve = "0"
fromRoleToInt R.Adam = "1"

-- if the node is deadend (means that the string argument is empty), we add an edge into dummy nodes.
isDeadEnd :: Int -> R.Role -> String -> String 
isDeadEnd i R.Eve ""  = show (i)
isDeadEnd i R.Adam "" = show (i + 1)
isDeadEnd _ _ s       = s

descriptNode :: Int -> R.ROG -> String 
descriptNode i (m, n, p, e, r, w) = case i < m of 
                                         True  -> show (i) ++ " " ++ (show (0)) ++ " " ++ (fromRoleToInt (R.Eve)) ++ " " ++ (succersors i (m, n, p, e, r, w)) ++ " " ++ "\"R.En" ++ show (i+1) ++ "\";" ++ "\n" 
                                                  ++ (descriptNode (i+1) (m, n, p, e, r, w))
                                         False -> case i < m + p of
                                                       True -> show (i) ++ " " ++ (show (w V.! (i - m))) ++ " " ++ (fromRoleToInt (r V.! (i - m))) ++ " " ++ (isDeadEnd (m+n+p) (r V.! (i - m)) (succersors i (m, n, p, e, r, w))) ++ " " ++ "\"R.INode"++show (i - m + 1)++ "\";" ++ "\n" 
                                                               ++ (descriptNode (i+1) (m, n, p, e, r, w))
                                                       False -> case i < m + p + n of 
                                                                     True -> show (i) ++ " " ++ (show (-1)) ++ " " ++ (fromRoleToInt (R.Eve)) ++ " " ++ "emp" ++ " " ++ "\"R.Ex" ++show (i - m - p + 1) ++ "\";" ++ "\n" 
                                                                             ++ (descriptNode (i+1) (m, n, p, e, r, w))
                                                                     False -> ""
    
    

            
    
                                                


