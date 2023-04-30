-- module ParserOfFreeProp where 
module ParserOfFreeProp(parserOfFreeProp) where 
import SyntaxOfFreeProp
import Text.Parsec
import ToolsForParser

-- the content of parser is the func expr
parserOfFreeProp :: Parsec String () Expr
parserOfFreeProp = expr

expr :: Parsec String () Expr
expr = do x <- term
          do {char ';'; y <- term; return $ SComp x y} 
             <|> do {char '+'; y <- term; return $ MProd x y} 
             <|> pure x

term ::  Parsec String () Expr
term = do termId
          <|> termSyn
          <|> termUnit
          <|> termCounit
          <|> termNode
          <|> parens expr

termId :: Parsec String () Expr
termId = do string "id_"
            ob <- letter
            let dom = direction ob
            return $ Id dom

termSyn :: Parsec String () Expr
termSyn = do string "b_("
             ob1 <- letter 
             char ','
             ob2 <- letter
             char ')'
             let dom1 = direction ob1
             let dom2 = direction ob2
             return $ Syn dom1 dom2

termUnit :: Parsec String () Expr
termUnit = do string "d_"
              ob <- letter
              let dom = direction ob
              return $ Unit dom

termCounit :: Parsec String () Expr
termCounit = do string "e_"
                ob <- letter
                let dom = direction ob
                return $ Counit dom

termNode :: Parsec String () Expr
termNode = do string "n^{"
              prerole <- letter
              char ','
              polarity <- many (char '-')
              upperPart <- many (digit)
              char '}'
              string "_{"
              ob1 <- many (char 'r' <|> char 'l')
              char ','
              ob2 <- many (char 'r' <|> char 'l')
              char '}'
              let dom1 = directions ob1
              let dom2 = directions ob2
              let role = fromCharToRole prerole
              let weight = convertToWeight polarity upperPart 
              return $ Node role weight dom1 dom2


