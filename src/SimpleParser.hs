{-# LANGUAGE OverloadedStrings #-}

module SimpleParser where

import Text.Parsec hiding (token)
import Text.Parsec.String (Parser)

data Expr
    = Var String
    | IntLit Int
    | StrLit String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Assign String Expr
    | If Expr [Expr] [Expr] 
    | Print Expr
    | ForLoop Expr Expr Expr [Expr]
    | Eq Expr Expr  -- Equal (==)
    | Neq Expr Expr -- Not equal( !=)
    | Lt Expr Expr -- Less than (<)
    | Gt Expr Expr -- Greater than (>)
    | Le Expr Expr -- Less or Equal (<=)
    | Ge Expr Expr -- Greater or Equal (>=)
    | BoolLit Bool 
    | ListLit [Expr]  
    | ListAccess Expr Expr
    | ListAppend Expr Expr
    | ListRemove Expr Expr
    | ListPop Expr Expr
    | ListAdd Expr Expr Expr
    deriving (Show, Eq)

token :: Parser a -> Parser a
token p = p <* spaces

symbol :: String -> Parser String
symbol = token . string

identifier :: Parser String
identifier = token $ (:) <$> letter <*> many alphaNum

integer :: Parser Int
integer = token $ read <$> many1 digit

stringLiteral :: Parser String
stringLiteral = token $ char '"' *> many (noneOf "\"") <* char '"'

boolLiteral :: Parser Bool
boolLiteral = token $ (True <$ string "true") <|> (False <$ string "false")

listLiteral :: Parser Expr
listLiteral = do
    _ <- symbol "["
    elements <- expr `sepBy` symbol ","
    _ <- symbol "]"
    return $ ListLit elements

listAccess :: Parser Expr
listAccess = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol "["
    index <- expr
    _ <- symbol "]"
    return $ ListAccess list index

-- TODO: Implement so it works with multiple appends and make the list mutable (?)
listAppend :: Parser Expr
listAppend = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol "<<"
    element <- expr
    return $ ListAppend list element

listRemove :: Parser Expr
listRemove = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol ">>"
    index <- expr
    return $ ListRemove list index

listPop :: Parser Expr
listPop = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol "=>>"
    index <- expr
    return $ ListPop list index

listAdd :: Parser Expr
listAdd = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol "<<"
    index <- expr

    _ <- symbol "<<="
    element <- expr

    return $ ListAdd list index element

expr :: Parser Expr
expr = try  listAdd
    <|> try listAppend 
    <|> try listRemove
    <|> try listPop
    <|> try listAccess 
    <|> try listLiteral
    <|> try comparison `chainl1` addSubOp
    <|> term

comparison :: Parser Expr
comparison = try (Eq <$> term <* symbol "==" <*> term)
        <|> try (Neq <$> term <* symbol "!=" <*> term)
        <|> try (Lt <$> term <* symbol "<" <*> term)
        <|> try (Gt <$> term <* symbol ">" <*> term)
        <|> try (Le <$> term <* symbol "<=" <*> term)
        <|> try (Ge <$> term <* symbol ">=" <*> term)
        <|> term  

term :: Parser Expr
term = factor `chainl1` mulDivOp

factor :: Parser Expr
factor =
    BoolLit <$> boolLiteral
    <|> Var <$> identifier
    <|> IntLit <$> integer
    <|> StrLit <$> stringLiteral
    <|> between (symbol "(") (symbol ")") expr

addSubOp :: Parser (Expr -> Expr -> Expr)
addSubOp =   (Add <$ symbol "+")
         <|> (Sub <$ symbol "-")

mulDivOp :: Parser (Expr -> Expr -> Expr)
mulDivOp =   (Mul <$ symbol "*")
         <|> (Div <$ symbol "/")

statement :: Parser Expr
statement =
        try assignment
    <|> forStatement
    <|> ifStatement
    <|> printStatement

assignment :: Parser Expr
assignment = do
    name <- identifier
    _ <- symbol "="
    val <- expr
    return $ Assign name val

ifStatement :: Parser Expr
ifStatement = do
    _ <- symbol "if"
    cond <- expr
    _ <- symbol "{"
    thenBranch <- many statement  
    _ <- symbol "}"
    elseBranch <- option [] $ do -- Optional else branch
        _ <- symbol "else"
        _ <- symbol "{"
        many statement <* symbol "}"
    return $ If cond thenBranch elseBranch

forStatement :: Parser Expr
forStatement = do
    _ <- symbol "for"
    _ <- symbol "("
    counterVariable <- assignment  
    _ <- symbol ";"
    cond <- expr  
    _ <- symbol ";"
    incr <- assignment  
    _ <- symbol ")"
    _ <- symbol "{"
    body <- many statement  
    _ <- symbol "}"
    return $ ForLoop counterVariable cond incr body

printStatement :: Parser Expr
printStatement = do
    _ <- symbol "print"
    val <- expr
    return $ Print val

program :: Parser [Expr]
program = spaces *> many1 statement <* eof

parseProgram :: String -> Either ParseError [Expr]
parseProgram = parse program ""
