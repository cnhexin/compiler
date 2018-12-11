module ASTInterpreter where

import Ast
import StatefulUnsafeMonad


data State = Map String Integer-- TODO change to be the type of state, you have freedom for how you implement it
 


-- evalSt :: Stmts -> StatefulUnsafe State String
-- evalSt (Assign var ex) = do res <- eval ex
                            --return ""   
--                        right <- evalInt r
--                        return left + right




-- *****************************************
-- helper functions
evalInt :: Expr -> StatefulUnsafe State Integer
evalInt a = do res <- evalEx a
               case res of
                 I i -> return i
                 _   -> err "This is not an Integer"
				 
evalS :: Expr -> StatefulUnsafe State String
evalS a = do res <- evalEx a
             case res of
               S i -> return i
               _   -> err "This is not an String"
			   
evalBool :: Expr -> StatefulUnsafe State Bool
evalBool a = do res <- evalEx a
                case res of
                     B i -> return i
                     _   -> err "This is not an Bool"

-- ******************************************
evalEx :: Expr -> StatefulUnsafe State Op
evalEx (Val num) = return $ I num
evalEx (Var s) = return $ S s
evalEx (Plus a b) = do left <- evalInt a
                       right <- evalInt b
                       return $ I $ left + right
evalEx (Minus l r) = do left <- evalInt l
                        right <- evalInt r
                        return $ I $ left - right
evalEx (Times l r) = do left <- evalInt l
                        right <- evalInt r
                        return $ I $ left * right
evalEx (Div l r) = do left <- evalInt l
                      right <- evalInt r
                      case right of
                           0 -> err "divid by 0"
                           _ -> return $ I $ left `div` right
evalEx (Mod l r) = do left <- evalInt l
                      right <- evalInt r
                      case right of
                           0 -> err "Mod by 0"
                           _ -> return $ I $ left `mod` right
evalEx (And l r) = do left <- evalBool l
                      right <- evalBool r
                      return $ B $ left && right
evalEx (Or l r) = do left <- evalBool l
                     right <- evalBool r
                     return $ B $ left || right
evalEx (Not r) = do right <- evalBool r
                    return $ B $ not right
evalEx (Eq l r) = do left <- evalBool l
                     right <- evalBool r
                     return $ B $ left == right
evalEx (NotEq l r) = do left <- evalBool l
                        right <- evalBool r
                        return $ B $ left /= right
evalEx (Lt l r) = do left <- evalBool l
                     right <- evalBool r
                     return $ B $ left < right
evalEx (Le l r) = do left <- evalBool l
                     right <- evalBool r
                     return $ B $ left <= right
evalEx (Gt l r) = do left <- evalBool l
                     right <- evalBool r
                     return $ B $ left > right
evalEx (Ge l r) = do left <- evalBool l
                     right <- evalBool r
                     return $ B $ left >= right
evalEx (Rev v) = do v <- evalInt v
                    return $ I $ negate v

-- eval (Let x l r) = do left <- eval l
--                       withVal x left (eval r)
-- eval (App f a) = do res1 <- evalFun f
--                     res2 <- eval a
--                     case res1 res2 of
-- 					     Error s    -> err s
-- 					     Ok resresa -> return resresa
-- eval (If x y z) = do xres <- evalBool x
--                      yres <- eval y
--                      zres <- eval z
--                      case xres of
--                           True  -> return yres
--                           False -> return zres






eval :: Program -> [String]
eval p = undefined
-- eval p = getPrints $ snd $ app (eval' p) undefined
--
-- getPrints :: State -> [String]
-- getPrints = undefined
--
-- eval' :: Program -> StatefulUnsafe State Int
-- eval' = undefined
