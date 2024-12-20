module Interpreter where

import SimpleParser
import qualified Data.Map as Map
import Control.Monad.Except
import Data.List (intercalate)
import Data.Text (splitOn)
import qualified Data.Text as T
import Control.Monad (foldM)
import Control.Monad.State
import System.Directory (doesFileExist)

{-
    The interpreter is the one that will evaluate the AST generated by the parser.
    It will evaluate the expressions and return the result of the evaluation.

    The interpreter is a monad transformer that combines the ExceptT monad transformer
-}

data Value =
    IntVal Int
    -- ^ It represents an integer value
    | StrVal String
    -- ^ It represents a string value
    | BoolVal Bool
    -- ^ It represents a boolean value
    | ListVal [Value]
    -- ^ It represents a list of values
    | NullVal
    -- ^ It represents a null value
    | FuncVal [String] [Expr] Env
    -- ^ It represents a function value
    | NextVal
    -- ^ It represents a next value
    | BreakVal
    -- ^ It represents a break value
    | ObjectVal [(String, Value)]
    -- ^ It represents an object value
    deriving (Show, Eq)


-- | The 'Env' type represents the environment of the interpreter, meaning where state is stored.
type Env = Map.Map String Value
-- | The 'Interpreter' type represents the interpreter monad transformer.
type Interpreter a = ExceptT String (StateT Env IO) a


{- |
The eval function is the one that will evaluate the AST generated by the parser.
It will evaluate the expressions and return the result of the evaluation.

The eval function is a recursive function that will evaluate the expressions and return the result of the evaluation.
The function will pattern match on the different types of expressions and evaluate them accordingly.

The function will also keep track of the environment, which is a map of variable names to values.

@param expr the expression to evaluate
@returns the result of the evaluation
@throws an error if the evaluation fails
-}
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

eval (ListLit elems) = do
    vals <- mapM eval elems
    return $ ListVal vals

eval (ListAccess listExpr indexExpr) = do
    listVal <- eval listExpr
    indexVal <- eval indexExpr
    case (listVal, indexVal) of
        (ListVal lst, IntVal idx) ->
            if idx >= 0 && idx < length lst
            then return $ lst !! idx
            else throwError "Index out of bounds"
        _ -> throwError "Type mismatch in list access"

eval (ListAppend listExpr elemExpr) = do
    listVal <- eval listExpr
    elemVal <- eval elemExpr
    case listVal of
        ListVal lst -> return $ ListVal (lst ++ [elemVal])
        _ -> throwError "Type mismatch in list append"

eval (ListRemove listExpr indexExpr) = do
    listVal <- eval listExpr
    indexVal <- eval indexExpr
    case (listVal, indexVal) of
        (ListVal lst, IntVal idx) ->
            if idx >= 0 && idx < length lst
            then return $ ListVal (take idx lst ++ drop (idx + 1) lst)
            else throwError "Index out of bounds"
        _ -> throwError "Type mismatch in list remove"

eval (ListPop listExpr indexExpr) = do
    listVal <- eval listExpr
    indexVal <- eval indexExpr
    case (listVal, indexVal) of
        (ListVal lst, IntVal idx) ->
            if idx >= 0 && idx < length lst
            then do
                let (left, right) = splitAt idx lst
                let poppedValue = head right
                modify (Map.insert (getListName listExpr) (ListVal (left ++ tail right)))
                return poppedValue
            else throwError "Index out of bounds"
        _ -> throwError "Type mismatch in list pop"

eval (ListAdd listExpr indexExpr elemExpr) = do
    listVal <- eval listExpr
    indexVal <- eval indexExpr
    elemVal <- eval elemExpr
    case (listVal, indexVal) of
        (ListVal lst, IntVal idx) ->
            if idx >= 0 && idx <= (length lst) 
            then do
                let (left, right) = splitAt idx lst
                let newList = left ++ [elemVal] ++ right
                return $ ListVal newList
            else throwError "Index out of bounds"
        _ -> throwError "Type mismatch in list add"

eval (FuncDef name params body) = do
    env <- get
    modify (Map.insert name (FuncVal params body env))
    return NullVal

eval (FuncCall name args) = do
    env <- get
    case Map.lookup name env of
        Just (FuncVal params body closure) -> do
            argVals <- mapM eval args
            let funcEnv = Map.union (Map.fromList (zip params argVals)) closure
            result <- withEnv funcEnv $ evalBlock body
            return result
        _ -> throwError $ "Undefined function: " ++ name
  where
    withEnv newEnv action = do
        oldEnv <- get
        put newEnv
        result <- action
        put oldEnv
        return result

eval (Return exprs) = eval exprs

eval (LogicAnd v1 v2) = do
    val1 <- eval v1
    val2 <- eval v2
    case (val1, val2) of
        (StrVal s1, StrVal s2) -> return $ BoolVal (s1 /= "" && s2 /= "")
        (IntVal n1, IntVal n2) -> return $ BoolVal (n1 /= 0 && n2 /= 0)
        (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 && b2)
        _ -> throwError "Type mismatch in and"

eval (LogicOr v1 v2) = do
    val1 <- eval v1
    val2 <- eval v2
    case (val1, val2) of
        (StrVal s1, StrVal s2) -> return $ BoolVal (s1 /= "" || s2 /= "")
        (IntVal n1, IntVal n2) -> return $ BoolVal (n1 /= 0 || n2 /= 0)
        (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 || b2)
        _ -> throwError "Type mismatch in or"

eval (LogicNot v) = do
    val <- eval v
    case val of
        StrVal s -> return $ BoolVal (s == "")
        IntVal n -> return $ BoolVal (n == 0)
        BoolVal b -> return $ BoolVal (not b)
        _ -> throwError "Type mismatch in not"

eval (ImportModule name) = do
    liftIO $ putStrLn $ "Importing module: " ++ name

    env <- get

    exists <- liftIO $ doesFileExist (name ++ ".aha")

    if not exists
        then do
            _ <- throwError $ "Module " ++ name ++ " does not exist"
            return NullVal
        else do
            moduleCode <- liftIO $ parseProgram <$> readFile (name ++ ".aha")
            case moduleCode of
                Left err -> liftIO $ putStrLn $ "Error while importing module " ++ name ++ ": " ++ show err
                Right ast -> do
                    _ <- evalBlock ast

                    newEnv <- get
                    put (Map.union newEnv env)
            return NullVal

eval (WhileLoop cond body) = do
    evalWhile cond body

eval (DoWhileLoop cond body) = do
    _ <- evalBlock body
    evalWhile cond body

eval Break = return BreakVal

eval Next = return NextVal

eval (ListRange start end optionalStep) = do
    startVal <- eval start
    endVal <- eval end
    stepVal <- case optionalStep of
        Just step -> eval step
        Nothing -> return $ IntVal 1

    case (startVal, endVal, stepVal) of
        (IntVal s, IntVal e, IntVal st) -> return $ ListVal [IntVal x | x <- [s, s + st .. e]]
        _ -> throwError "Type mismatch in list range"

eval (ObjectDef properties) = do
    vals <- mapM evalObjectProperty properties
    return $ ObjectVal vals
  where
    evalObjectProperty (name, exprs) = do
        val <- eval exprs
        return (name, val)

eval (ObjectAccess objExpr prop) = do
    objVal <- eval objExpr
    case objVal of
        ObjectVal props -> case lookup prop props of
            Just val -> return val
            Nothing -> throwError $ "Property " ++ prop ++ " not found"
        _ -> throwError "Type mismatch in object access"

-- TODO: Implement so it alters in place and does not alter the order of the properties
eval (ObjectSet objExpr prop valExpr) = do
    objVal <- eval objExpr
    valVal <- eval valExpr
    case objVal of
        ObjectVal props -> do
            let filteredProps = removeNestedProperty props (map T.unpack (splitPropName (T.pack prop)))
            let newProps = setNestedProperty filteredProps (map T.unpack (splitPropName (T.pack prop))) valVal
            modify (Map.insert (getObjectName objExpr) (ObjectVal newProps))
            return $ ObjectVal newProps
        _ -> throwError "Type mismatch in object set"
  where
    splitPropName = splitOn (T.pack ".")

    -- Takes a prop list and a list of properties to remove
    removeNestedProperty :: [(String, Value)] -> [String] -> [(String, Value)]
    removeNestedProperty props [p] = filter (\(k, _) -> k /= p) props
    removeNestedProperty props [] = props
    removeNestedProperty props (p:rest) = case lookup p props of
        Just (ObjectVal nestedProps) -> 
            let updatedNestedProps = removeNestedProperty nestedProps rest
            in (p, ObjectVal updatedNestedProps) : filter (\(k, _) -> k /= p) props
        Just _ -> error "Cannot remove nested property on non-object"
        Nothing -> props
    
    -- Takes a prop list and searches for insert point
    setNestedProperty :: [(String, Value)] -> [String] -> Value -> [(String, Value)]
    setNestedProperty props [p] val = props ++ [(p, val)]
    setNestedProperty props [] _ = props
    setNestedProperty props (p:rest) val = case lookup p props of
        Just (ObjectVal nestedProps) -> (p, ObjectVal (setNestedProperty nestedProps rest val)) : filter (\(k, _) -> k /= p) props
        Just _ -> error "Cannot set nested property on non-object"
        Nothing -> (p, ObjectVal (setNestedProperty [] rest val)) : filter (\(k, _) -> k /= p) props

eval (StringInterpolation strs exprs) = do
    let exprsVals = mapM eval exprs
    exprVals <- exprsVals
    let strVals = map (valueToStringInterpolated) exprVals
    let interpolatedStr = concat $ intercalate [""] (zipWith (\s e -> [s, e]) strs strVals)
    return $ StrVal interpolatedStr

-- | The 'getListName' function returns the name of the list if is a variable.
getListName :: Expr -> String
getListName (Var name) = name
getListName _ = error "Expected a variable name for list"

-- | The 'getObjectName' function returns the name of the object if it is a variable.
getObjectName :: Expr -> String
getObjectName (Var name) = name
getObjectName x = error $ "Expected a variable name for object, but got: " ++ show x

{- |
The evalWhile function is the one that will evaluate the while loop expression.
It will evaluate the condition and body of the while loop and return the result of the evaluation.

@param condition the condition of the while loop
@param body the body of the while loop
@returns the result of the evaluation
@throws an error if the evaluation fails
-}
evalWhile :: Expr -> [Expr] -> Interpreter Value
evalWhile condition body = do
    condVal <- eval condition
    case condVal of
        BoolVal True -> do
            result <- evalBlockWithControl body
            case result of
                BreakVal -> return NullVal  
                NextVal -> evalWhile condition body 
                _ -> evalWhile condition body 
        BoolVal False -> return NullVal 
        _ -> throwError "Error while evaluating while loop: invalid condition"

{- |
The evalForLoop function is the one that will evaluate the for loop expression.
It will evaluate the condition, increment and body of the for loop and return the result of the evaluation.
-}
evalForLoop :: Expr -> Expr -> [Expr] -> Interpreter Value
evalForLoop cond incr body = do
    condVal <- eval cond
    case condVal of
        BoolVal True -> do
            result <- evalBlockWithControl body
            case result of
                BreakVal -> return NullVal  
                NextVal -> do
                    _ <- eval incr  
                    evalForLoop cond incr body
                _ -> do
                    _ <- eval incr
                    evalForLoop cond incr body
        BoolVal False -> return NullVal 
        _ -> throwError "Error while evaluating for loop: invalid condition"


-- | The 'evalBlock' function evaluates a block of expressions.
evalBlock :: [Expr] -> Interpreter Value
evalBlock = foldM (\_ exprs -> eval exprs) NullVal

-- | The 'evalBlockWithControl' function evaluates a block of expressions with control.
evalBlockWithControl :: [Expr] -> Interpreter Value
evalBlockWithControl [] = return NullVal
evalBlockWithControl (expression:expressions) = do
    result <- eval expression
    case result of
        BreakVal -> return BreakVal  
        NextVal -> return NextVal    
        _ -> evalBlockWithControl expressions        

-- | The 'valueToString' function converts a value to a string.
valueToString :: Value -> String
valueToString (IntVal n)   = show n
valueToString (StrVal s)   = "\"" ++ s ++ "\""
valueToString (BoolVal b)  = if b then "true" else "false"
valueToString NullVal      = "null"
valueToString (ListVal l)  = "[" ++ (intercalate ", " (map valueToString l)) ++ "]"
valueToString (FuncVal _ _ _) = "<function>"
valueToString NextVal      = "next"
valueToString BreakVal     = "haltLoop"
valueToString (ObjectVal props) = "{" ++ (intercalate ", " (map (\(name, val) -> name ++ ": " ++ valueToString val) props)) ++ "}"

valueToStringInterpolated :: Value -> String
valueToStringInterpolated (StrVal s)   = s
valueToStringInterpolated otherVal = valueToString otherVal

{- |
The runInterpreter function is the one that will run the interpreter.
It will take an initial environment and a list of expressions to evaluate.
It will return the result of the evaluation.

The function will run the interpreter and return the result of the evaluation.
The function will also return the final environment after the evaluation.
-}
runInterpreter :: Env -> [Expr] -> IO (Either String (Value, Env))
runInterpreter initialEnv exprs = do
    (result, finalEnv) <- runStateT (runExceptT (evalBlock exprs)) initialEnv
    return $ case result of
        Left err -> Left err
        Right _ -> Right (IntVal 0, finalEnv) -- return val 0 meaning good exit code
