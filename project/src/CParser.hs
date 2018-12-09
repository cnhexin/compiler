module CParser where

import Ast
import ParserMonad

keywords = ["if","then","else", "let", "in", "true","false"]

parser :: Parser Program
parser = (do s <- parserS
             rest <- parser
             return (s:rest))
             <||> do s <- parserS
                     return [s]

parserE :: Parser Expr
parserE = addSubExpr <||> multDivExpr <||> notExp <||> atoms

parserS :: Parser Stmts
parserS = whileParser <||> assignParser


ints :: Parser Expr
ints = do i <- token $ intParser
          return $ Val i
		  
vars :: Parser Expr
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s
		  
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
atoms = ints <||> parens <||> vars

parens :: Parser Expr
parens = do token (literal "(")
            res <- parserE
            token (literal ")")
            return res
			
assignParser :: Parser Stmts
assignParser = do l <- token $ varParser
                  token $ literal "="
                  r <- token $ parserE
                  token $ literal ";"
                  return $ Assign l r
whileParser :: Parser Stmts
whileParser = do token $ literal "while"
                 token $ literal "("
                 l <- token $ parserE
                 token $ literal ")"
                 token $ literal "{"
                 r <- token $ parserS
                 token $ literal "}"
                 return $ While l r	

ifParser :: Parser Stmts
ifParser = do token $ literal "if"
              l <- token $ parserE
              r <- token $ parserS
              return $ If l r

ifElseParser :: Parser Stmts
ifElseParser = undefined
                
               				 


