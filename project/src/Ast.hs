module Ast where

type Program = [Stmts] -- TODO: your ast here

-- TODO: Ast should have at least Eq and Show instances

data Stmts = Assign String Expr |
             While Expr Stmts |
             Block [Stmts] |
             If Expr Stmts |
             Else Stmts 
             deriving Eq
       

data Expr = Val Integer | Var String |
            Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr |
			Not Expr
            deriving Eq

--eval :: Expr -> Integer
--eval (Val x) = x
--eval (Plus x y) = eval x + eval y 

--instance Show Expr where
  --show (Val x) = show x
  --show (Plus x y) = show (eval x) ++ "+" ++ show (eval y)
prettyShowS :: Stmts -> String
prettyShowS (Assign s a) = "Assign " ++ s ++ " " ++ (prettyShowE a)
prettyShowS (While a s) = "While " ++ (prettyShowE a) ++ " " ++ (prettyShowS s)
 
prettyShowE :: Expr -> String
prettyShowE (Val i) = if i < 0
                      then  "(" ++ show i ++ ")"
                      else show i
prettyShowE (Plus x y) = (prettyShowE x) ++ " + " ++ (prettyShowE y)
prettyShowE (Minus x y) = (prettyShowE x) ++ " - " ++ (prettyShowE y)
prettyShowE (Times x y) = (prettyShowE x) ++ " * " ++ (prettyShowE y)
prettyShowE (Div x y) = (prettyShowE x) ++ " / " ++ (prettyShowE y)
prettyShowE (Not x) = " ! " ++ (prettyShowE x)