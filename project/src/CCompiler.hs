module CCompiler where

import Ast
import CParser
import ASTInterpreter
import ICInterpreter
import TestsProject
import ParserMonad
import Data.Map(Map, lookup, insert, empty, fromList, findWithDefault)
import StatefulUnsafeMonad

-- map for functions, @param function_name, function_arguments
type Symbol_table = Map String (Integer, [String], Stmts)
-- initialized temp ts
temps = ["_t" ++ show n | n <- [1..]]
testC = ([Push',Call' 3,Halt'],take 10 temps,([],[],[],[]),Data.Map.empty)
-- backpatching list, True, False, Break, Continue
type BP = ([Integer], [Integer], [Integer], [Integer])
-- state for monad, IC_program, temps, symbol_table
type Compile_State = (IC_Program, [String], BP, Symbol_table)
-- output of compile Exprs
-- type ExprOut = (String, IC_Program, BP, [String])
-- ****************************************************************************
-- helper functions:
getAdress :: String -> Symbol_table -> Integer
getAdress s table = case Data.Map.lookup s table of
                         Just (i,_,_) -> i
                         _ -> undefined
getStmts :: String -> Symbol_table -> [Stmts]
getStmts s table = case Data.Map.lookup s table of
                        Just (_,_,Block st) -> st 
                        _ -> undefined
changeStatePlus :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStatePlus op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Plus' (Var' t) op1 op2)], ts, bp, symbol))
changeStateMinus :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateMinus op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Minus' (Var' t) op1 op2)], ts, bp, symbol))
changeStateTimes :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateTimes op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Times' (Var' t) op1 op2)], ts, bp, symbol))
changeStateDiv :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateDiv op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Div' (Var' t) op1 op2)], ts, bp, symbol))
changeStateMod :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateMod op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Mod' (Var' t) op1 op2)], ts, bp, symbol))
changeStateLt :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateLt op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Lt' (Var' t) op1 op2)], ts, bp, symbol))
changeStateGt :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateGt op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Gt' (Var' t) op1 op2)], ts, bp, symbol))
changeStateLe :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateLe op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Le' (Var' t) op1 op2)], ts, bp, symbol))
changeStateGe :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateGe op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Ge' (Var' t) op1 op2)], ts, bp, symbol))
changeStateEq :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateEq op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(Equal' (Var' t) op1 op2)], ts, bp, symbol))
changeStateNotEq :: Op -> Op -> StatefulUnsafe Compile_State Op
changeStateNotEq op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [(NotEq' (Var' t) op1 op2)], ts, bp, symbol))
changeStateUminus :: Op -> StatefulUnsafe Compile_State Op
changeStateUminus op = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t), (ic ++ [Uminus' (Var' t) op], ts, bp, symbol))
changeStateAssign :: String -> Op -> StatefulUnsafe Compile_State Op
changeStateAssign s op = StatefulUnsafe $ \(ic, t, bp, symbol) -> (Ok (Var' s), (ic ++ [Assign' (Var' s) op], t, bp, symbol))
changeStatePrint :: String -> StatefulUnsafe Compile_State Op
changeStatePrint s = StatefulUnsafe $ \(ic, t, bp, symbol) -> (Ok (Var' s), (ic ++ [Print' (s++" = ") (Var' s)], t, bp, symbol))
insertFunction :: String -> [String] -> Stmts -> StatefulUnsafe Compile_State Op
insertFunction s [] st = StatefulUnsafe $ \(ic, t, bp , symbol) -> (Ok (Var' "func"), (ic, t, bp, insert s (fromIntegral (length (ic)),[],st) symbol))
changeStateReturn :: Op -> StatefulUnsafe Compile_State Op
changeStateReturn op = StatefulUnsafe $ \(ic, t, bp, symbol) -> (Ok op,(ic ++ [Return' op], t, bp, symbol))
-- insertFunction s [x:xs] st = 
-- ****************************************************************************

compile :: Program -> IC_Program
compile p = compileMain (compileFuncs p testC)
compileFuncs :: Program -> Compile_State -> Compile_State
compileFuncs [] state = state
compileFuncs (x:xs) state = case app (compileStmt x) state of
                                 (_,newState) -> compileFuncs xs newState
compileMain :: Compile_State -> IC_Program
compileMain (ic, temp, bp, symbol) =  compileStmts (getStmts "main" symbol) (ic,temp,bp,symbol)
compileStmts :: [Stmts] -> Compile_State -> IC_Program
compileStmts [] (ic, temp, bp, symbol) =  ic
compileStmts (s:sx) (ic, temp, bp, symbol) = case app (compileStmt s) (ic, temp, bp, symbol) of
                                                  (_,(newIc, newTemp,newBp,_)) -> compileStmts sx (newIc,newTemp,newBp,symbol)
-- data Stmts = Assign String Expr |
--              While Expr Stmts |
--              Block [Stmts] |
--              If Expr Stmts |
--              IfElse Expr Stmts Stmts |
--        Func String Expr Stmts |
--              FuncNoArg String Stmts |
--              Return Expr |
--              Print String |
--              Break |
--              Continue 
--              deriving Eq
-- --  TODO : Arg [Expr] eval?
-- data Expr = 
--             And Expr Expr |
--             Or Expr Expr |
--             Not Expr |
--             Arg [Expr] |
--       Call String Expr |
--       CallNoArg String

compileStmt :: Stmts -> StatefulUnsafe Compile_State Op
compileStmt (Assign s e) = do op <- compileExpr e
                              changeStateAssign s op
compileStmt (Print s) = changeStatePrint s
compileStmt (FuncNoArg s stmt) = insertFunction s [] stmt
compileStmt (Return e) = do res <- compileExpr e
                            changeStateReturn res
compileStmt _ = undefined
compileExpr :: Expr -> StatefulUnsafe Compile_State Op
compileExpr (Val i) = return (Val' (fromInteger i))
compileExpr (Var s) = return (Var' s)
compileExpr (Plus expr1 expr2) = do res1 <- compileExpr expr1
                                    res2 <- compileExpr expr2
                                    changeStatePlus res1 res2
compileExpr (Minus expr1 expr2) = do res1 <- compileExpr expr1
                                     res2 <- compileExpr expr2
                                     changeStateMinus res1 res2
compileExpr (Times expr1 expr2) = do res1 <- compileExpr expr1
                                     res2 <- compileExpr expr2
                                     changeStateTimes res1 res2
compileExpr (Div expr1 expr2) = do res1 <- compileExpr expr1
                                   res2 <- compileExpr expr2
                                   changeStateDiv res1 res2
compileExpr (Mod expr1 expr2) = do res1 <- compileExpr expr1
                                   res2 <- compileExpr expr2
                                   changeStateMod res1 res2
compileExpr (Lt expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateLt res1 res2
compileExpr (Gt expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateGt res1 res2
compileExpr (Le expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateLe res1 res2
compileExpr (Ge expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateGe res1 res2
compileExpr (Eq expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateEq res1 res2
compileExpr (NotEq expr1 expr2) = do res1 <- compileExpr expr1
                                     res2 <- compileExpr expr2
                                     changeStateNotEq res1 res2
compileExpr (NotEq expr1 expr2) = do res1 <- compileExpr expr1
                                     res2 <- compileExpr expr2
                                     changeStateNotEq res1 res2
compileExpr (Rev expr) = do res <- compileExpr expr
                            changeStateUminus res


compileExpr _ = undefined 



test_compile :: String -> IC_Program
test_compile s = compile (changeToProgram (parse parser s))





