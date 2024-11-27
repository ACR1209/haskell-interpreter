{-# LANGUAGE FlexibleContexts #-}

module Main where

import Interpreter
import SimpleParser
import qualified Data.Map as Map
import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--ast", fileName] -> processFile fileName True
        [fileName]          -> processFile fileName False
        _                   -> putStrLn "Usage: executable [--ast] <source-file>"

processFile :: FilePath -> Bool -> IO ()
processFile fileName printAst = do
    programText <- readFile fileName
    case parseProgram programText of
        Left err -> putStrLn $ "\n\n\n\nSyntax Error detected:\n" ++ err ++ "\n\n\n\n"
        Right ast -> do
            _ <- when printAst $ print ast
            result <- runInterpreter Map.empty ast
            case result of
                Left evalErr -> putStrLn $ "Runtime error: " ++ evalErr
                Right (_, _) -> return ()
