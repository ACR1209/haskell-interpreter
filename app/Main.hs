{-# LANGUAGE FlexibleContexts #-}

module Main where

import Interpreter
import SimpleParser
import qualified Data.Map as Map
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            programText <- readFile fileName
            case parseProgram programText of
                Left err -> print err
                Right ast -> do
                    _ <- print ast
                    result <- runInterpreter Map.empty ast
                    case result of
                        Left evalErr -> putStrLn $ "Runtime error: " ++ evalErr
                        Right (_, _) -> return ()
        _ -> putStrLn "Usage: executable <source-file>"
