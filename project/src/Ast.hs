module Ast where

data Program -- TODO: your ast here

-- TODO: Ast should have at least Eq and Show instances

data Stmts = Assign String Expr |
             While Expr Stmts |
             Block [Stmts] |
             If Expr Stmts |
             Else Stmts 
             deriving Eq
       

data Expr = ValInt Integer |
            Plus Expr Expr | Minus Expr Expr | Mult Expr Expr | Div Expr Expr
            deriving Eq
eval :: Expr -> Integer
eval (ValInt x) = x
eval (Plus x y) = eval x + eval y 
instance Show Expr where
  show (ValInt x) = show x
  show (Plus x y) = show (eval x) ++ "+" ++ show (eval y)
prettyShow :: Program -> String
prettyShow = undefined