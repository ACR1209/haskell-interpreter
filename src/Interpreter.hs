module Interpreter where

import SimpleParser
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad (foldM)
import Control.Monad.State


data Value =
    IntVal Int
    | StrVal String
    | BoolVal Bool
    | NullVal
    deriving (Show, Eq)

type Env = Map.Map String Value
type Interpreter a = ExceptT String (StateT Env IO) a

eval :: Expr -> Interpreter Value
eval (Var name) = do
    env <- get
    case Map.lookup name env of
        Just val -> return val
        Nothing -> throwError $ "Undefined variable: " ++ name

eval (IntLit n) = return $ IntVal n
eval (StrLit s) = return $ StrVal s
eval (BoolLit val) = return $ BoolVal (val)

eval (Add e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (IntVal n1, IntVal n2) -> return $ IntVal (n1 + n2)
        (StrVal s1, StrVal s2) -> return $ StrVal (s1 ++ s2) 
        _ -> throwError "Type mismatch in addition"

eval (Sub e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (IntVal n1, IntVal n2) -> return $ IntVal (n1 - n2)
        _ -> throwError "Type mismatch in subtraction"

eval (Mul e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (IntVal n1, IntVal n2) -> return $ IntVal (n1 * n2)
        _ -> throwError "Type mismatch in multiplication"

eval (Div e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (IntVal n1, IntVal n2) ->
            if n2 == 0
                then throwError "Division by zero"
                else return $ IntVal (n1 `div` n2)
        _ -> throwError "Type mismatch in division"

eval (Assign name express) = do
    val <- eval express
    modify (Map.insert name val)
    return val

eval (If cond thenBranch elseBranch) = do
    condVal <- eval cond
    case condVal of
        BoolVal n | n /= False -> evalBlock thenBranch
        BoolVal _ -> evalBlock elseBranch
        IntVal n | n /= 0 -> evalBlock thenBranch
        IntVal _ -> evalBlock elseBranch
        StrVal n | n /= "" -> evalBlock thenBranch
        StrVal _ -> evalBlock elseBranch
        _ -> throwError "Error while evaluating if: invalid condition"

eval (Print express) = do
    val <- eval express
    liftIO $ putStrLn (valueToString val)
    return NullVal

eval (Eq exp1 exp2) = do
    v1 <- eval exp1
    v2 <- eval exp2

    case (v1, v2) of
        (IntVal r1, IntVal r2) -> return $ BoolVal (r1 == r2) 
        (StrVal r1, StrVal r2) -> return $ BoolVal (r1 == r2) 
        (BoolVal r1, BoolVal r2) -> return $ BoolVal (r1 == r2)
        _ -> throwError $ "Cannot compare the two values (==), v1: " ++ show v1 ++ "; v2: " ++ show v2


eval (Neq exp1 exp2) = do
    v1 <- eval exp1
    v2 <- eval exp2

    case (v1, v2) of
        (IntVal r1, IntVal r2) -> return $ BoolVal (r1 /= r2) 
        (StrVal r1, StrVal r2) -> return $ BoolVal (r1 /= r2)
        (BoolVal r1, BoolVal r2) -> return $ BoolVal (r1 /= r2)
        _ -> throwError $ "Cannot compare the two values (!=), v1: " ++ show v1 ++ "; v2: " ++ show v2

eval (Lt exp1 exp2) = do
    v1 <- eval exp1
    v2 <- eval exp2

    case (v1, v2) of
        (IntVal r1, IntVal r2) -> return $ BoolVal (r1 < r2) 
        _ -> throwError $ "Cannot compare the two values (<), v1: " ++ show v1 ++ "; v2: " ++ show v2

eval (Gt exp1 exp2) = do
    v1 <- eval exp1
    v2 <- eval exp2

    case (v1, v2) of
        (IntVal r1, IntVal r2) -> return $ BoolVal (r1 > r2) 
        _ -> throwError $ "Cannot compare the two values (>), v1: " ++ show v1 ++ "; v2: " ++ show v2

eval (Le exp1 exp2) = do
    v1 <- eval exp1
    v2 <- eval exp2

    case (v1, v2) of
        (IntVal r1, IntVal r2) -> return $ BoolVal (r1 <= r2) 
        _ -> throwError $ "Cannot compare the two values (<=), v1: " ++ show v1 ++ "; v2: " ++ show v2

eval (Ge exp1 exp2) = do
    v1 <- eval exp1
    v2 <- eval exp2

    case (v1, v2) of
        (IntVal r1, IntVal r2) -> return $ BoolVal (r1 >= r2) 
        _ -> throwError $ "Cannot compare the two values (>=), v1: " ++ show v1 ++ "; v2: " ++ show v2

eval (ForLoop counterVariable cond incr body) = do
    _ <- eval counterVariable
    
    evalForLoop cond incr body

evalForLoop :: Expr -> Expr -> [Expr] -> Interpreter Value
evalForLoop cond incr body = do
    condVal <- eval cond
    case condVal of
        BoolVal n -> do
            if n == False
                then return $ BoolVal False
                else do
                    _ <- evalBlock body
                    _ <- eval incr

                    evalForLoop cond incr body
        
        _ -> throwError "Error while evaluating for loop: invalid condition"

evalBlock :: [Expr] -> Interpreter Value
evalBlock = foldM (\_ exprs -> eval exprs) NullVal

valueToString :: Value -> String
valueToString (IntVal n)   = show n
valueToString (StrVal s)   = s
valueToString (BoolVal b)  = if b then "true" else "false"
valueToString NullVal      = "null"

runInterpreter :: Env -> [Expr] -> IO (Either String (Value, Env))
runInterpreter initialEnv exprs = do
    (result, finalEnv) <- runStateT (runExceptT (evalBlock exprs)) initialEnv
    return $ case result of
        Left err -> Left err
        Right _ -> Right (IntVal 0, finalEnv) -- return val 0 meaning good exit code
