{-# LANGUAGE OverloadedStrings #-}

module SimpleParser where

import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)


type Parser = Parsec Void String

data Expr
    = Var String 
    -- ^ Represents a variable in the AHA language, the string is the variable name  
    | IntLit Int 
    -- ^ Represents an integer literal in the AHA language
    | StrLit String 
    -- ^ Represents a string literal in the AHA language
    | BoolLit Bool 
    -- ^ Represents a boolean literal in the AHA language
    | Add Expr Expr 
    -- ^ Represents an addition operation in the AHA language
    | Sub Expr Expr 
    -- ^ Represents a subtraction operation in the AHA language
    | Mul Expr Expr 
    -- ^ Represents a multiplication operation in the AHA language
    | Div Expr Expr 
    -- ^ Represents a division operation in the AHA language 
    | Assign String Expr 
    -- ^ Represents an assignment operation in the AHA language. The string is the variable name, the Expr is the value to assign.
    | If Expr [Expr] [Expr] 
    -- ^ Represents an if statement in the AHA language. The first Expr is the condition, the second list of Exprs is the then branch, the third list of Exprs is the else branch.
    | Print Expr 
    -- ^ Represents a print statement in the AHA language. The Expr is the value to print.
    | ForLoop Expr Expr Expr [Expr] 
    -- ^ Represents a for loop in the AHA language. The first Expr is the counter variable, the second Expr is the condition, the third Expr is the increment, the list of Exprs is the body of the loop. 
    | Eq Expr Expr  
    -- ^ Equal (==)
    | Neq Expr Expr 
    -- ^ Not equal( !=)
    | Lt Expr Expr 
    -- ^ Less than (<)
    | Gt Expr Expr 
    -- ^ Greater than (>)
    | Le Expr Expr 
    -- ^ Less or Equal (<=)
    | Ge Expr Expr 
    -- ^ Greater or Equal (>=)
    | LogicAnd Expr Expr 
    -- ^ Represents the logical operator and in the AHA language. It compares that two Expr evaluate to True.
    | LogicOr Expr Expr 
    -- ^ Represents the logical operator or in the AHA language. It checks that of two Expr one evaluates to True.  
    | LogicNot Expr 
    -- ^ Represents the logical operator not in the AHA language. It inverts the boolean value of an Expr
    | ListLit [Expr]  
    -- ^ Represents a list in the AHA language. A list is composed of Expr
    | ListAccess Expr Expr 
    -- ^ Represents the accessing to an index of the list. The first Expr is the list and the second is the index.
    | ListAppend Expr Expr 
    -- ^ Represents adding and element to the end of the list. The first Expr is the list and the second is the element to append.
    | ListRemove Expr Expr 
    -- ^ Represents the removal of an index from a list. The first Expr is the list and the second is the index to remove.
    | ListPop Expr Expr 
    -- ^ Represents the operation of removing an element from a list and return it. The first element is the list and the second the index to pop.
    | ListAdd Expr Expr Expr  
    -- ^ Represents the operation of adding an element to a list at a specific index. The first Expr is the list, the second is the index and the third is the element to add.
    | ImportModule String 
    -- ^ Represents the import of a module in the AHA language. The string is the path of the module.
    | FuncDef String [String] [Expr]  
    -- ^ Represents a function definition in the AHA language. Function definition: name, parameters, body
    | FuncCall String [Expr]      
    -- ^ Represents a function call in the AHA language. Function call: name, arguments
    | Return Expr 
    -- ^ Represents a return statement in the AHA language. The Expr is the value to return.    
    | DoWhileLoop Expr [Expr]
    -- ^ Represents a do while loop in the AHA language. The first Expr is the condition, the list of Exprs is the body of the loop.
    | WhileLoop Expr [Expr]
    -- ^ Represents a while loop in the AHA language. The first Expr is the condition, the list of Exprs is the body of the loop.  
    | Next 
    -- ^ Represents the next statement in the AHA language.   
    | Break
    -- ^ Represents the break statement in the AHA language.   
    | ListRange Expr Expr (Maybe Expr)
    -- ^ Represents a range of elements in a list. The first Expr is the start index, the second is the end index, the third is the step.     
    deriving (Show, Eq)

{-
    The parser is implemented using the Parsec library. The parser is a monadic parser that parses the AHA language.
-}


{-
    ##################################
    #       Basic definitions        #
    ##################################
-}

{- |    
The 'token' function takes a parser 'p' and returns a new parser that
applies 'p' and then consumes any trailing whitespace characters.
This is useful for tokenizing input where spaces are not significant.

@param p The parser to be applied.
@return A new parser that applies 'p' and then consumes trailing spaces.
-}
token :: Parser a -> Parser a
token p = spaceConsumer *> p <* spaceConsumer

{- | 
Parses a specific symbol (string) from the input.
The 'symbol' function takes a 'String' and returns a 'Parser String'.
It uses the 'token' combinator to ensure that the parsed string is treated as a single token,
and the 'string' parser to match the exact sequence of characters.

@param s The string to be parsed as a symbol.
@return A parser that matches the given string.
-}
symbol :: String -> Parser String
symbol = token . string


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

{-|
Parses an identifier, which is defined as a letter followed by zero or more alphanumeric characters.
The parser consumes any leading whitespace or comments before parsing the identifier.

@param s The string to be parsed as an identifier.
@return A parser that produces a string representing the identifier.
-}
identifier :: Parser String
identifier = token $ (:) <$> letterChar <*> many alphaNumChar

{-|
Parses an integer from the input. Consumes one or more digits from the input and converts them
into an integer. It uses the 'token' combinator to handle any leading
whitespace and the 'many1' combinator to ensure that at least one digit
is present in the input.

@param s The string to be parsed as an integer.
@return  A parser that produces an 'Int' from the input.
-}
integer :: Parser Integer
integer = token L.decimal

{-|
Parses a string literal from the input. The parser consumes a sequence of characters enclosed in double quotes.
The 'many' combinator is used to parse zero or more characters between the quotes.

@param s The string to be parsed as a string literal.
@return A parser that produces a string representing the string literal.
-}
stringLiteral :: Parser String
stringLiteral =  token (char '"' >> manyTill L.charLiteral (char '"'))

{-|
Parses a boolean literal from the input. The parser consumes the string "true" or "false" and returns the corresponding boolean value.

@param s The string to be parsed as a boolean literal.
@return A parser that produces a 'Bool' value.
-}
boolLiteral :: Parser Bool
boolLiteral = (True <$ string "true") <|> (False <$ string "false")


{-|
Parses an assignment operation from the input. The parser consumes an identifier followed by the '=' operator and an expression.

@param s The string to be parsed as an assignment operation.
@return A parser that produces an expression representing the assignment operation.
-}
assignment :: Parser Expr
assignment = do
    name <- identifier
    _ <- L.symbol spaceConsumer "="
    val <- expr
    return $ Assign name val

-- Helper to parse left-associative binary expressions
parseLeftAssoc :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
parseLeftAssoc base op = do
    left <- base
    rest left
  where
    rest left = (do
        operator <- op
        right <- base
        rest (operator left right)) <|> return left

addSubExpr :: Parser Expr
addSubExpr = parseLeftAssoc mulDivExpr addSubOp

mulDivExpr :: Parser Expr
mulDivExpr = parseLeftAssoc term mulDivOp


{-
    #######################################
    #       Arithmetic definitions        #
    #######################################
-}

{-|
Parses an addition or subtraction operator from the input.

@return A parser that produces an expression representing an addition or subtraction operation.
-}
addSubOp :: Parser (Expr -> Expr -> Expr)
addSubOp =   (Add <$ token (symbol "+"))
         <|> (Sub <$ token (symbol "-"))


{-|
Parses a multiplication or division operator from the input.

@return A parser that produces an expression representing a multiplication or division operation.
-}
mulDivOp :: Parser (Expr -> Expr -> Expr)
mulDivOp =   (Mul <$ token (symbol "*"))
         <|> (Div <$ token (symbol "/"))


{-
    #################################
    #       List definitions        #
    #################################
-}

{-|
Parses a list literal from the input. The parser consumes a sequence of expressions enclosed in square brackets.

@param s The string to be parsed as a list literal.
@return A parser that produces a list of expressions.
-}
listLiteral :: Parser Expr
listLiteral = do
    elements <- between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")
    return $ ListLit elements


{-|
Parses a list access operation from the input. The parser consumes a list or a variable followed by an index enclosed in square brackets.

@param s The string to be parsed as a list access operation.
@return A parser that produces an expression representing the list access operation.
-}
listAccess :: Parser Expr
listAccess = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    index <- between (symbol "[") (symbol "]") expr
    return $ ListAccess list index

-- TODO: Implement so it works with multiple appends LogicAnd make the list mutable (?)

{-|
Parses a list append operation from the input. The parser consumes a list or a variable followed by the '<<' operator and an expression.

@param s The string to be parsed as a list append operation.
@return A parser that produces an expression representing the list append operation.
-}
listAppend :: Parser Expr
listAppend = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol "<<"
    element <- expr
    return $ ListAppend list element

{-|
Parses a list remove operation from the input. The parser consumes a list or a variable followed by the '>>' operator and an index.

@param s The string to be parsed as a list remove operation.
@return A parser that produces an expression representing the list remove operation.
-}
listRemove :: Parser Expr
listRemove = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol ">>"
    index <- expr
    return $ ListRemove list index

{-|
Parses a list pop operation from the input. The parser consumes a list or a variable followed by the '=>>' operator and an index.

@param s The string to be parsed as a list pop operation.
@return A parser that produces an expression representing the list pop operation.
-}
listPop :: Parser Expr
listPop = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol "=>>"
    index <- expr
    return $ ListPop list index

{-|
Parses a list add operation from the input. The parser consumes a list or a variable followed by the '<<' operator, an index, the '<<=' operator, and an element.

@param s The string to be parsed as a list add operation.
@return A parser that produces an expression representing the list add operation.
-}
listAdd :: Parser Expr
listAdd = do
    list <- try listLiteral <|> (Var  <$> (try identifier)) 
    _ <- symbol "<<"
    index <- expr

    _ <- symbol "<<="
    element <- expr

    return $ ListAdd list index element


rangeStatement :: Parser Expr
rangeStatement = do
    start <- term
    _ <- symbol ".."
    end <- term
    step <- optional $ do
        _ <- symbol "stepping"
        term
    return $ ListRange start end step

{-
    ######################################
    #       Functions definitions        #
    ######################################
-}

{-|
Parses a function definition from the input. The parser consumes the keyword 'def' followed by the function name, parameters enclosed in parentheses, and the function body enclosed in curly braces.

@param s The string to be parsed as a function definition.
@return A parser that produces an expression representing the function definition.
-}
functionDef :: Parser Expr
functionDef = do
    _ <- symbol "def"
    name <- identifier
    params <- between (symbol "(") (symbol ")") (identifier `sepBy` symbol ",")
    body <- between (symbol "{") (symbol "}") (many functionStatement)
    return $ FuncDef name params body

{-|
Parses a function call from the input. The parser consumes the function name followed by arguments enclosed in parentheses.

@param s The string to be parsed as a function call.
@return A parser that produces an expression representing the function call.
-}
functionCall :: Parser Expr
functionCall = do
    name <- identifier
    args <- between (symbol "(") (symbol ")") (expr `sepBy` symbol ",")
    return $ FuncCall name args

{-|
Parses a return statement from the input. The parser consumes the keyword 'return' followed by an expression.

@param s The string to be parsed as a return statement.
@return A parser that produces an expression representing the return statement.
-}
returnStatement :: Parser Expr
returnStatement = do
    _ <- symbol "return"
    val <- expr
    return $ Return val

{-|
Parses a function statement from the input. The parser consumes a return statement or a regular statement.

@param s The string to be parsed as a function statement.
@return A parser that produces an expression representing the function statement.
-}
functionStatement :: Parser Expr
functionStatement = try returnStatement
                <|> try statement

{-
    ###################################
    #       Import definitions        #
    ###################################
-}


-- For now it will support import of whole files and at the start of a file and as relative path from the root directory of the compiler
-- TODO: Make it so it's relative to current file

{-|
Parses an import statement from the input. The parser consumes the keyword 'import' followed by the relative path of the module to import.

@param s The string to be parsed as an import statement.
@return A parser that produces an expression representing the import statement.
-}
importModule :: Parser Expr
importModule = do
    _ <- symbol "import"
    relativeModulePathString <- many (noneOf ("\n" :: String))
    _ <- optional (symbol "\n")
    return $ ImportModule relativeModulePathString

{-
    #######################################
    #       Comparison definitions        #
    #######################################
-}

{-|
Parses a comparison operation from the input. The parser consumes two expressions separated by a comparison operator.

@param s The string to be parsed as a comparison operation.
@return A parser that produces an expression representing the comparison operation.
-}
comparison :: Parser Expr
comparison = try (Eq <$> term <* token (symbol "==") <*> term)
        <|> try (Neq <$> term <* token (symbol "!=") <*> term)
        <|> try (Lt <$> term <* token (symbol "<") <*> term)
        <|> try (Gt <$> term <* token (symbol ">") <*> term)
        <|> try (Le <$> term <* token (symbol "<=") <*> term)
        <|> try (Ge <$> term <* token (symbol ">=") <*> term)

{-
    ###############################################
    #       Logical operations definitions        #
    ###############################################
-}

{-|
Parses a logical operation from the input. The parser consumes two expressions separated by a logical operator.

@param s The string to be parsed as a logical operation.
@return A parser that produces an expression representing the logical operation.
-}
logicalOps :: Parser Expr
logicalOps = try (LogicAnd <$> term <* symbol "and" <*> term )
        <|> try (LogicOr <$> term <* symbol "or" <*> term)
        <|> try (LogicNot <$> (symbol "not" *> term))


{-
    #########################################
    #       If statement definitions        #
    #########################################
-}

{-|
Parses an if statement from the input. The parser consumes the keyword 'if' followed by a condition, the then branch enclosed in curly braces, and an optional else branch.

@param s The string to be parsed as an if statement.
@return A parser that produces an expression representing the if statement.
-}
ifStatement :: Parser Expr
ifStatement = do
    _ <- symbol "if"
    cond <- expr
    thenBranch <- between (symbol "{") (symbol "}") (many statement)  
    elseBranch <- option [] $ do -- Optional else branch
        _ <- symbol "else"
        between (symbol "{") (symbol "}") (many statement)
    return $ If cond thenBranch elseBranch


{-
    ##########################################
    #       For statement definitions        #
    ##########################################
-}

{-|
Parses a for loop statement from the input. The parser consumes the keyword 'for' followed by a counter variable assignment, a condition, an increment operation, and the body of the loop enclosed in curly braces.

@param s The string to be parsed as a for loop statement.
@return A parser that produces an expression representing the for loop statement.
-}
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
    body <-between (symbol "{") (symbol "}") (many loopControlStatement)  
    return $ ForLoop counterVariable cond incr body

{-
    #######################################
    #       While loop definitions        #
    #######################################
-}

{-|
Parses a do-while loop statement from the input. The parser consumes the keyword 'do' followed by the body of the loop enclosed in curly braces and the keyword 'loop' followed by the loop condition.

@param s The string to be parsed as a do-while loop statement.
@return A parser that produces an expression representing the do-while loop statement.
-}
doWhileLoop :: Parser Expr
doWhileLoop = do
    _ <- symbol "do"
    body <- between (symbol "{") (symbol "}") (many loopControlStatement)
    _ <- symbol "loop"
    cond <- expr
    return $ DoWhileLoop cond body

{-|
Parses a while loop statement from the input. The parser consumes the keyword 'loop' followed by the loop condition and the body of the loop enclosed in curly braces.

@param s The string to be parsed as a while loop statement.
@return A parser that produces an expression representing the while loop statement.
-}
whileLoop :: Parser Expr
whileLoop = do
    _ <- symbol "loop"
    cond <- expr
    body <- between (symbol "{") (symbol "}") (many loopControlStatement)
    return $ WhileLoop cond body

{-
    ####################################################
    #       Loop control statements definitions        #
    ####################################################
-}

{-|
Parses a next if statement from the input. The parser consumes the keyword 'next' followed by the keyword 'if' and a condition.
    
@param s The string to be parsed as a next if statement.
@return A parser that produces an expression representing the next if statement.
-}
nextIfStatement :: Parser Expr
nextIfStatement = do
    _ <- symbol "next"
    _ <- symbol "if"
    cond <- expr
    return $ If cond [Next] []

{-|
Parses a next unless statement from the input. The parser consumes the keyword 'next' followed by the keyword 'unless' and a condition.

@param s The string to be parsed as a next unless statement.
@return A parser that produces an expression representing the next unless statement.
-}
nextUnlessStatement :: Parser Expr
nextUnlessStatement = do
    _ <- symbol "next"
    _ <- symbol "unless"
    cond <- expr
    return $ If cond [] [Next]

{-|
Parses a next statement from the input. The parser consumes the keyword 'next'.

@param s The string to be parsed as a next statement.
@return A parser that produces an expression representing the next statement.
-}
nextStatement :: Parser Expr
nextStatement = do
    _ <- symbol "next"
    return Next

{-|
Parses a break statement from the input. The parser consumes the keyword 'haltLoop'.

@param s The string to be parsed as a break statement.
@return A parser that produces an expression representing the break statement.
-}
breakStatement :: Parser Expr
breakStatement = do
    _ <- symbol "haltLoop"
    return Break

loopControlStatement :: Parser Expr
loopControlStatement = try nextIfStatement
                    <|> try nextUnlessStatement
                    <|> try breakStatement
                    <|> try nextStatement
                    <|> try statement

{-
    ##########################################
    #       IO statements definitions        #
    ##########################################
-}

{-|
Parses a print statement from the input. The parser consumes the keyword 'print' followed by an expression to print.

@param s The string to be parsed as a print statement.
@return A parser that produces an expression representing the print statement.
-}
printStatement :: Parser Expr
printStatement = do
    _ <- symbol "print"
    val <- expr
    return $ Print val


{-
    ########################################
    #       High level definitions        #
    ########################################
-}

{-|
Parses an expression from the input. The parser consumes a term followed by an optional chain of binary operators.

@param s The string to be parsed as an expression.
@return A parser that produces an expression.
-}
expr :: Parser Expr
expr = try functionCall
    <|> try rangeStatement
    <|> try listAdd
    <|> try listAppend
    <|> try listRemove
    <|> try listPop
    <|> try listAccess
    <|> try logicalOps
    <|> try listLiteral
    <|> try comparison
    <|> try addSubExpr
    <|> try mulDivExpr
    <|> try term

{-|
Parses a term from the input. The parser consumes a factor followed by an optional chain of multiplication/division operators.

@param s The string to be parsed as a term.
@return A parser that produces an expression representing the term.
-}
term :: Parser Expr
term = try factor

{-|
Parses a factor from the input. The parser consumes a boolean literal, an identifier, an integer literal, a string literal, or an expression enclosed in parentheses.

@param s The string to be parsed as a factor.
@return A parser that produces an expression representing the factor.
-}
factor :: Parser Expr
factor =
    BoolLit <$> boolLiteral
    <|> Var <$> identifier
    <|> IntLit . fromInteger <$> integer
    <|> StrLit <$> stringLiteral
    <|> between (symbol "(") (symbol ")") expr

{-|
Parses a statement from the input. The parser consumes different kinds of statements, such as assignments, function calls, if statements, for loops, and print statements.

@param s The string to be parsed as a statement.
@return A parser that produces an expression representing the statement.
-}
statement :: Parser Expr
statement = try assignment
        <|> try whileLoop
        <|> try importModule
        <|> try functionCall
        <|> try rangeStatement
        <|> try doWhileLoop
        <|> try forStatement
        <|> try ifStatement
        <|> try listLiteral
        <|> try printStatement
        <|> try functionDef

{-
    #########################################
    #       Parse runner definitions        #
    #########################################
-}

{-|
Parses a program from the input. The parser consumes multiple statements separated by whitespace.

@param s The string to be parsed as a program.
@return A parser that produces a list of expressions representing the program.
-}
program :: Parser [Expr]
program = spaceConsumer *> many statement <* eof

{-|
Parses a program from a string input. The function takes a string representing the program and returns either a list of expressions or a parse error.

@param s The string to be parsed as a program.
@return Either a list of expressions or a parse error.
-}
parseProgram :: String -> Either String [Expr]
parseProgram input = case parse program "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result
