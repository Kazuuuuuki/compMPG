module ToolsForParser where

import Text.Parsec

parens :: Parsec String () a -> Parsec String () a
parens p = do char '('
              x <- p
              char ')'
              return x
            
bigParens :: Parsec String () a -> Parsec String () a
bigParens p = do char '{'
                 x <- p
                 char '}'
                 return x
            