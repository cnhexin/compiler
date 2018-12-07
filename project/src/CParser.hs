module CParser where

import Ast
import ParserMonad

parser :: Parser Program
parser = undefined

ints :: Parser Expr
ints = do i <- token $ intParser
          return $ ValInt i
		  
addSubExpr :: Parser Expr
addSubExpr = withInfix multDivExpr [("+",Plus),("-", Minus)]

multDivExpr :: Parser Expr
multDivExpr = withInfix notExp [("*",Times),("/",Div)]

notExp :: Parser Expr
notExp = undefined

atoms :: Parser Expr
atoms = ints 

parens :: Parser Program
parens = do token (literal "(")
            res <- parser
            token (literal ")")
            return res	 


