module ParserOfGame where 
import SyntaxOfFreeProp as FP
import ParserOfFreeProp
import qualified SyntaxOfGraph as SG
import qualified ParserOfFreeProp as PFP
import qualified CompactClosedSemanticCategory as CC
import qualified Interpretation as Inter
import qualified Realization as R
import qualified OpenGame as O
import System.IO
import Text.Parsec
import ToolsForParser
import Data.List.Split
import Debug.Trace

-- syntax parsing for contexts
parserOfGame :: Parsec String () SG.Smallgraph
parserOfGame = expr

expr :: Parsec String () SG.Smallgraph
expr = do x <- term
          do {char '.'; y <- term; return $ SG.SCompgraph x y} 
             <|> pure x

term :: Parsec String () SG.Smallgraph
term = do termBase
          <|> bigParens expr

termBase :: Parsec String () SG.Smallgraph
termBase = do x <- termBaseGraph
              return $ SG.Base x

termBaseGraph :: Parsec String () SG.Base
termBaseGraph = do x <- baseterm
                   do {char ';'; y <- baseterm; return $ SG.SComp x y} 
                      <|> do {char '+'; y <- baseterm; return $ SG.PComp x y} 
                      <|> pure x

baseterm :: Parsec String () SG.Base
baseterm = do termVar
              <|> parens termBaseGraph

termVar :: Parsec String () SG.Base
termVar = do 
             char 'v'
             char '_'
             char '{'
             num <- many (digit)
             char '}'
             let var = "v_{" ++ num ++ "}"
             return $ SG.Var var


-- semantic parsing for contexts
parseFreeProp :: String -> Either String CC.Mor 
parseFreeProp s = case parse parserOfFreeProp "Invalid Syntax" s of 
                       Right ex -> Right (Inter.interpretation ex)
                       Left s -> Left "parse error: invalid syntax for free prop"

-- used for semantic computation
parseValuation :: [String] -> Either String SG.Valuation
parseValuation [] = Right []
parseValuation (x:xs) = case parseFreeProp g of 
                             Right m -> case pv of 
                                             Right val -> Right ((v, m):val)
                                             Left s  -> Left s
                             Left s -> Left s
                        where l = splitOn ":" x 
                              v = head l 
                              g = head (tail l)
                              pv = parseValuation xs

-- used for translation to the parity games
readValuation :: [String] -> Either String [(SG.Var, FP.Expr)]
readValuation [] = Right []
readValuation (x:xs) = case parse parserOfFreeProp "Invalid Syntax" g of 
                            Right m -> case pv of 
                                            Right val -> Right ((v, m):val)
                                            Left s  -> Left s
                            Left s -> Left "parse error"
                        where l = splitOn ":" x 
                              v = head l 
                              g = head (tail l)
                              pv = readValuation xs


-- sometimes, we call context as formContext.
parseScompFormContext :: Int -> [String] -> Either ParseError SG.Smallgraph
parseScompFormContext 1 l = parse parserOfGame "invalid smallgraph" (head l)
parseScompFormContext n l = case parseScompFormContext (n-1) (tail l) of
                                 Right ex1 -> case parse parserOfGame "invalid smallgraph" (head l) of 
                                                   Right ex2 -> Right (SG.SCompgraph ex2 ex1)
                                                   Left s -> Left s
                                 Left s -> Left s

parsePcompFormContext :: Int -> [String] -> Either ParseError SG.Smallgraph
parsePcompFormContext 1 l = parse parserOfGame "invalid smallgraph" (head l)
parsePcompFormContext n l = case parsePcompFormContext (n-1) (tail l) of
                                 Right ex1 -> case parse parserOfGame "invalid smallgraph" (head l) of 
                                                   Right ex2 -> Right (SG.PCompgraph ex2 ex1)
                                                   Left s -> Left s
                                 Left s -> Left s
                                 

parseInputData :: [String] -> Either String CC.Mor
parseInputData s = case op of 
                        "SComp" -> case y of 
                                        Right ex -> case evars of 
                                                         Right v -> SG.interpretation (ex, v)
                                                         Left  s -> Left s
                                        Left _   -> Left "parse error"
                                        where y = parseScompFormContext size (tail (tail s)) 
                        _       -> case y of 
                                        Right ex -> case evars of 
                                                         Right v -> SG.interpretation (ex, v)
                                                         Left  s -> Left s
                                        Left _   -> Left "parse error"
                                        where y = parsePcompFormContext size (tail (tail s)) 

                    where size = read (head s) :: Int
                          op = head (tail s)
                          vars = drop (size+2) s 
                          evars = parseValuation vars

-- parsing input file
-- parseFile :: IO()
-- parseFile = do path <- getLine
--                handle <- openFile path ReadMode
--                contents <- hGetContents handle
--                let l = lines contents
--                let m = parseInputData l
--                putStrLn $ CC.printMorEither m


-- used for translation from contexts to parity games
parseToFP :: [String] -> Either String FP.Expr
parseToFP s = case op of 
                        "SComp" -> case y of 
                                        Right ex -> case evars of 
                                                         Right v -> SG.translation (ex, v)
                                                         Left s  ->  Left s
                                        Left _   -> Left "parse error"
                                        where y = parseScompFormContext size (tail (tail s)) 
                        _       -> case y of 
                                        Right ex -> case evars of 
                                                         Right v -> SG.translation (ex, v)
                                                         Left s  ->  Left s
                                        Left _   -> Left "parse error"
                                        where y = parsePcompFormContext size (tail (tail s)) 
              where size = read (head s) :: Int
                    op = head (tail s)
                    vars = drop (size+2) s
                    evars = readValuation vars




                  




          