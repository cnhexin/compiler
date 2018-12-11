module CParser where

import Ast
import ParserMonad

keywords = ["if","then","else", "let", "in", "true","false","def"]

parser :: Parser Program
parser = (do s <- parserS
             rest <- parser
             return (s:rest))
             <||> do s <- parserS
                     return [s]
					 
parserA :: Parser Arguments
parserA = (do s <- parserE
              rest <- parserA
              return (s:rest))
              <||> do s <- parserE
                      return [s]

parserE :: Parser Expr
parserE = addSubExpr <||> multDivExpr <||> notExp <||> atoms

parserS :: Parser Stmts
parserS = funcParser <||> whileParser <||> assignParser <||> ifElseParser <||> ifParser


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
			 
eqParser :: Parser Expr
eqParser = (do x <- parserE
               token $ literal "=="
               y <- parserE
               return $ Eq x y)
               <||> atoms
			   
notEqParser :: Parser Expr
notEqParser = (do x <- parserE
                  token $ literal "/="
                  y <- parserE
                  return $ NotEq x y)
                  <||> atoms
			   
ltParser :: Parser Expr
ltParser = (do x <- parserE
               token $ literal "<"
               y <- parserE
               return $ Lt x y)
               <||> atoms

gtParser :: Parser Expr
gtParser = (do x <- parserE
               token $ literal ">"
               y <- parserE
               return $ Gt x y)
               <||> atoms

leParser :: Parser Expr
leParser = (do x <- parserE
               token $ literal "<="
               y <- parserE
               return $ Le x y)
               <||> atoms
			   
geParser :: Parser Expr
geParser = (do x <- parserE
               token $ literal ">="
               y <- parserE
               return $ Ge x y)
               <||> atoms
			 
argParser :: Parser Expr
argParser = (do s <- parserE
                rest <- parserA
                return $ (Arg (s:rest)))
                <||> do s <- parserE
                        return $ (Arg [s])

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
                 r <- token $ blockParser
                 token $ literal "}"
                 return $ While l r	
				 
blockParser :: Parser Stmts
blockParser = (do s <- parserS
                  rest <- parser
                  return $ (Block (s:rest)))
                  <||> do s <- parserS
                          return $ (Block [s])
						  
ifParser :: Parser Stmts
ifParser = do token $ literal "if"
              token $ literal "("
              l <- token $ parserE
              token $ literal ")"
              token $ literal "{"
              r <- token $ blockParser
              token $ literal "}"
              return $ If l r

ifElseParser :: Parser Stmts
ifElseParser = do token $ literal "if"
                  token $ literal "("
                  x <- token $ parserE
                  token $ literal ")"
                  token $ literal "{"
                  y <- token $ blockParser
                  token $ literal "}"
                  token $ literal "else"
                  token $ literal "{"
                  z <- token $ blockParser
                  token $ literal "}"
                  return $ IfElse x y z
				  
funcParser :: Parser Stmts
funcParser = do token $ literal "def"
                x <- token $ varParser
                token $ literal "("
                y <- token $ argParser
                token $ literal ")"
                token $ literal "{"
                z <- token $ blockParser
                token $ literal "}"
                return $ Func x y z
                
               				 


