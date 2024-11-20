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

expr :: Parser Expr
expr = try term `chainl1` addSubOp

term :: Parser Expr
term = factor `chainl1` mulDivOp

factor :: Parser Expr
factor =
        Var <$> identifier
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

printStatement :: Parser Expr
printStatement = do
    _ <- symbol "print"
    val <- expr
    return $ Print val

program :: Parser [Expr]
program = spaces *> many1 statement <* eof

parseProgram :: String -> Either ParseError [Expr]
parseProgram = parse program ""
