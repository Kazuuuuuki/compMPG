module Realization where
import Text.Parsec
import SyntaxOfFreeProp as FP
import Data.Set as Set
import qualified Data.Map.Strict as M
import UsefulFuncs as Us
import Numeric.Natural
import qualified RightwardOpenGame as R
import qualified OpenGame as O
import qualified Data.Vector as V

-- -- realization functor from the free prop to the graph category
-- realization :: Expr -> O.Mor
-- realization (SComp e1 e2) = O.scomp (realization e1) (realization e2)
-- realization (MProd e1 e2) = O.pcomp (realization e1) (realization e2)
-- realization (Id DRight) = O.id (1, 0)
-- realization (Id DLeft) = O.id (0, 1)
-- realization (Syn DRight DRight) = O.sym (1, 0) (1, 0)
-- realization (Syn DRight DLeft) = O.sym (1, 0) (0, 1)
-- realization (Syn DLeft DRight) = O.sym (0, 1) (1, 0)
-- realization (Syn DLeft DLeft) = O.sym (0, 1) (0, 1)
-- realization (Unit DRight) = ((0, 0), (1, 1), (1, 1, 0, Set.fromAscList [(R.En 1, R.Ex 1)], [], []))
-- realization (Unit DLeft) = ((0, 0), (1, 1), (1, 1, 0, Set.fromAscList [(R.En 1, R.Ex 1)], [], []))
-- realization (Counit DRight) = ((1, 1), (0, 0), (1, 1, 0, Set.fromAscList [(R.En 1, R.Ex 1)], [], []))
-- realization (Counit DLeft) = ((1, 1), (0, 0), (1, 1, 0, Set.fromAscList [(R.En 1, R.Ex 1)], [], []))
-- realization (Node Eve w dom codom) = ((numRight dom, numLeft dom), (numRight codom, numLeft codom), realizationOfEveNode (numRight dom + numLeft codom) (numRight codom + numLeft dom) w )
-- realization (Node Adam w dom codom) = ((numRight dom, numLeft dom), (numRight codom, numLeft codom), realizationOfAdamNode (numRight dom + numLeft codom) (numRight codom + numLeft dom) w )

-- realizationOfEveNode :: R.Obj -> R.Obj -> R.Weight -> R.ROG
-- realizationOfEveNode m n w = (m, n, 1, Set.fromAscList ([(R.En i, R.INode 1) | i <- [1..m]] ++ [(R.INode 1, R.Ex i) | i <- [1..n]]), [R.Eve], [w])

-- realizationOfAdamNode :: R.Obj -> R.Obj -> R.Weight -> R.ROG
-- realizationOfAdamNode m n w = (m, n, 1, Set.fromAscList ([(R.En i, R.INode 1) | i <- [1..m]] ++ [(R.INode 1, R.Ex i) | i <- [1..n]]), [R.Adam], [w])


-- realization functor from the free prop to the graph category
realization :: Expr -> O.Mor
realization (SComp e1 e2) = O.scomp (realization e1) (realization e2)
realization (MProd e1 e2) = O.pcomp (realization e1) (realization e2)
realization (Id DRight) = O.id (1, 0)
realization (Id DLeft) = O.id (0, 1)
realization (Syn DRight DRight) = O.sym (1, 0) (1, 0)
realization (Syn DRight DLeft) = O.sym (1, 0) (0, 1)
realization (Syn DLeft DRight) = O.sym (0, 1) (1, 0)
realization (Syn DLeft DLeft) = O.sym (0, 1) (0, 1)
realization (Unit DRight) = ((0, 0), (1, 1), (1, 1, 0, V.fromList [V.singleton 1], V.empty, V.empty))
realization (Unit DLeft) = ((0, 0), (1, 1), (1, 1, 0, V.fromList [V.singleton 1], V.empty, V.empty))
realization (Counit DRight) = ((1, 1), (0, 0), (1, 1, 0, V.fromList [V.singleton 1], V.empty, V.empty))
realization (Counit DLeft) = ((1, 1), (0, 0), (1, 1, 0, V.fromList [V.singleton 1], V.empty, V.empty))
realization (Node Eve w dom codom) = ((numRight dom, numLeft dom), (numRight codom, numLeft codom), realizationOfEveNode (numRight dom + numLeft codom) (numRight codom + numLeft dom) w )
realization (Node Adam w dom codom) = ((numRight dom, numLeft dom), (numRight codom, numLeft codom), realizationOfAdamNode (numRight dom + numLeft codom) (numRight codom + numLeft dom) w )

realizationOfEveNode :: R.Obj -> R.Obj -> R.Weight -> R.ROG
realizationOfEveNode m n w = (m, n, 1, V.fromList ([ V.singleton m| i <- [1..m]] ++ [V.generate n (\i -> (i+m+1))]), V.singleton R.Eve, V.singleton w)

realizationOfAdamNode :: R.Obj -> R.Obj -> R.Weight -> R.ROG
realizationOfAdamNode m n w = (m, n, 1, V.fromList ([ V.singleton m| i <- [1..m]] ++ [V.generate n (\i -> (i+m+1))]), V.singleton R.Adam, V.singleton w)


numRight :: FP.Obj -> R.Obj
numRight [] = 0
numRight (DRight:l) = 1 + (numRight l)
numRight (DLeft:l) = numRight l

numLeft :: FP.Obj -> R.Obj
numLeft [] = 0
numLeft (DLeft:l) = 1 + (numLeft l)
numLeft (DRight:l) = numLeft l