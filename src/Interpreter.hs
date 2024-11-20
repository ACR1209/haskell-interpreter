module Interpreter where

import SimpleParser
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad (foldM)
import Control.Monad.State

type Env = Map.Map String (Either Int String)
type Interpreter a = ExceptT String (StateT Env IO) a

eval :: Expr -> Interpreter (Either Int String)
eval (Var name) = do
    env <- get
    case Map.lookup name env of
        Just (Left intVal) -> return $ Left intVal
        Just (Right strVal) -> return $ Right strVal
        Nothing -> throwError $ "Undefined variable: " ++ name

eval (IntLit n) = return $ Left n
eval (StrLit s) = return $ Right s

eval (Add e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Left n1, Left n2) -> return $ Left (n1 + n2)
        (Right s1, Right s2) -> return $ Right (s1 ++ s2) 
        _ -> throwError "Type mismatch in addition"

eval (Sub e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Left n1, Left n2) -> return $ Left (n1 - n2)
        _ -> throwError "Type mismatch in subtraction"

eval (Mul e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Left n1, Left n2) -> return $ Left (n1 * n2)
        _ -> throwError "Type mismatch in multiplication"

eval (Div e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Left n1, Left n2) ->
            if n2 == 0
                then throwError "Division by zero"
                else return $ Left (n1 `div` n2)
        _ -> throwError "Type mismatch in division"

eval (Assign name express) = do
    val <- eval express
    modify (Map.insert name val)
    return val

eval (If cond thenBranch elseBranch) = do
    condVal <- eval cond
    case condVal of
        Left n | n /= 0 -> evalBlock thenBranch
        Left _ -> evalBlock elseBranch
        _ -> throwError "Condition must evaluate to an integer"

eval (Print express) = do
    val <- eval express
    case val of
        Left n -> liftIO $ print n       
        Right s -> liftIO $ putStrLn s   
    return $ Left 0                      


evalBlock :: [Expr] -> Interpreter (Either Int String)
evalBlock = foldM (\_ express -> eval express) (Left 0)

runInterpreter :: Env -> [Expr] -> IO (Either String (Int, Env))
runInterpreter initialEnv exprs = do
    (result, finalEnv) <- runStateT (runExceptT (evalBlock exprs)) initialEnv
    return $ case result of
        Left err -> Left err
        Right (Left _) -> Right (0, finalEnv)
        Right (Right _) -> Right (0, finalEnv)
