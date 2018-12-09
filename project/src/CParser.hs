module CParser where

import Ast
import ParserMonad

parser :: Parser Program
parser = undefined

parserE :: Parser Expr
parserE = undefined


ints :: Parser Expr
ints = do i <- token $ intParser
          return $ Val i
		  
addSubExpr :: Parser Expr
addSubExpr = withInfix multDivExpr [("+",Plus),("-", Minus)]

multDivExpr :: Parser Expr
multDivExpr = withInfix notExp [("*",Times),("/",Div)]

notExp :: Parser Expr
notExp = (do token $ literal "!"
             ares <- notExp
             return $ Not ares)
             <||> atoms

atoms :: Parser Expr
atoms = ints <||> parens

parens :: Parser Expr
parens = do token (literal "(")
            res <- parserE
            token (literal ")")
            return res
			
assignParser :: Parser Stmts
assignParser = undefined 			


