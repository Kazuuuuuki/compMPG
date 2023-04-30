module SyntaxOfFreeProp where 

-- free prop
data Direction 
    = DLeft
    | DRight
    deriving (Eq, Show, Read)

data Role
    = Eve
    | Adam
    deriving (Eq, Show, Read)

type Weight = Integer

    
data Expr 
    = SComp Expr Expr 
    | MProd Expr Expr
    | Id Direction
    | Syn Direction Direction
    | Unit Direction
    | Counit Direction
    | Node Role Weight Obj Obj
    deriving (Eq, Show, Read)

type Obj = [Direction]

-- in my implementation, gId [] is not an identity. But, this is not problematic.

gId :: Obj -> Expr 
gId [] = Node Eve 0 [] []
gId [DRight] = Id DRight 
gId [DLeft]  = Id DLeft 
gId ((DRight):(xs)) = MProd (Id DRight) (gId xs) 
gId ((DLeft):(xs)) = MProd (Id DLeft) (gId xs) 

rtrace :: Expr -> Obj -> Obj -> Expr 
rtrace e dom codom = SComp (SComp f1 f2) f3
                     where f1 = MProd (Unit DLeft) (gId dom)
                           f2 = MProd (Id DLeft) e
                           f3 = MProd (Counit DRight) (gId codom)
 
ltrace :: Expr -> Obj -> Obj -> Expr 
ltrace e dom codom = SComp (SComp f1 f2) f3
                     where f1 = MProd (Unit DRight) (gId dom)
                           f2 = MProd (Id DRight) e
                           f3 = MProd (Counit DLeft) (gId codom)

direction :: Char -> Direction
direction 'r' = DRight
direction _ = DLeft

opDirection :: Direction -> Direction 
opDirection DRight = DLeft
opDirection DLeft  = DRight 

directions :: String -> [Direction]
directions ('r':s) = DRight : directions s
directions ('l':s) = DLeft : directions s
directions _ = []

fromCharToRole :: Char -> Role
fromCharToRole 'e' = Eve
fromCharToRole _ = Adam

convertToWeight :: String -> String -> Integer
convertToWeight s u = read (s ++ u ) :: Integer

fromObjToString :: Obj -> String 
fromObjToString []          = ""
fromObjToString (x:xs) = case x of 
                            DRight -> "r" ++ (fromObjToString xs)
                            DLeft  -> "l" ++ (fromObjToString xs)

toString :: Expr -> String 
toString (Id DRight)             = "id_r"
toString (Id DLeft)              = "id_l"
toString (Syn DRight DRight)     = "b_(r,r)"
toString (Syn DRight DLeft)      = "b_(r,l)"
toString (Syn DLeft DRight)      = "b_(l,r)"
toString (Syn DLeft DLeft)       = "b_(l,l)"
toString (Unit DRight)           = "d_r"
toString (Unit DLeft)            = "d_l"
toString (Counit DRight)         = "e_r"
toString (Counit DLeft)          = "e_l"
toString (Node Eve r dom codom)  = "n^{e," ++ (show r) ++ "}" ++ "_{" ++ (fromObjToString dom) ++  "," ++ (fromObjToString codom) ++ "}"
toString (Node Adam r dom codom) = "n^{a," ++ (show r) ++ "}" ++ "_{" ++ (fromObjToString dom) ++  "," ++ (fromObjToString codom) ++ "}"
toString (SComp ex1 ex2)         = "(" ++ toString ex1 ++ ")" ++ ";" ++ "(" ++ toString ex2 ++ ")"
toString (MProd ex1 ex2)         = "(" ++ toString ex1 ++ ")" ++ "+" ++ "(" ++ toString ex2 ++ ")"


-- (id_r+n^{a,20.0}_{rl,lrl});(b_(r,l)+n^{a,324.0}_{rl,ll})