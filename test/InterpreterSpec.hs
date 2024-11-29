module InterpreterSpec where

import Test.Hspec
import SimpleParser
import Interpreter
import Data.Either (isLeft)
import qualified Data.Map as Map

spec :: Spec
spec = context "Interpreter" $ do
    describe "Basic operations" $ do
      it "should evaluate integer assignment" $ do
          let exprs = [Assign "x" (IntLit 42)]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42)])

      it "should evaluate string assignment" $ do
          let exprs = [Assign "x" (StrLit "hello")]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal "hello")])

      it "should evaluate addition of integers" $ do
          let exprs = [Assign "x" (Add (IntLit 42) (IntLit 10))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 52)])

      it "should evaluate string concatenation" $ do
          let exprs = [Assign "x" (Add (StrLit "hello") (StrLit "world"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal "helloworld")])

      it "should evaluate subtraction of integers" $ do
          let exprs = [Assign "x" (Sub (IntLit 42) (IntLit 10))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 32)])

      it "should evaluate multiplication of integers" $ do
          let exprs = [Assign "x" (Mul (IntLit 42) (IntLit 10))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 420)])

      it "should evaluate division of integers" $ do
          let exprs = [Assign "x" (Div (IntLit 42) (IntLit 10))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 4)])

      it "should handle division by zero" $ do
          let exprs = [Assign "x" (Div (IntLit 42) (IntLit 0))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Division by zero"

      it "should recover string variables" $ do
          let exprs = [Assign "x" (StrLit "hello"), Var "x"]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal "hello")])

      it "should recover integer variables" $ do
          let exprs = [Assign "x" (IntLit 42), Var "x"]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42)])

      it "should throw error if the variables is not defined" $ do
          let exprs = [Var "x"]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Undefined variable: x"
      
      it "should throw error if trying to add two different types" $ do
          let exprs = [Add (IntLit 42) (StrLit "hello")]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Type mismatch in addition"
      
      it "should throw error if trying to subtract two different types" $ do
          let exprs = [Sub (IntLit 42) (StrLit "hello")]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Type mismatch in subtraction"
      
      it "should throw error if trying to multiply two different types" $ do
          let exprs = [Mul (IntLit 42) (StrLit "hello")]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Type mismatch in multiplication"
      
      it "should throw error if trying to divide two different types" $ do
          let exprs = [Div (IntLit 42) (StrLit "hello")]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Type mismatch in division"

    describe "If statement" $ do
        it "should evaluate if statement with true condition" $ do
            let exprs = [Assign "x" (IntLit 1), If (Var "x") [Assign "y" (IntLit 42)] [Assign "y" (IntLit 0)]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 1), ("y", IntVal 42)])

        it "should evaluate with bools" $ do
            let exprs = [Assign "x" (BoolLit True), If (Var "x") [Assign "y" (IntLit 42)] [Assign "y" (IntLit 0)]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True), ("y", IntVal 42)])

        it "should consider an empty string falsy" $ do
            let exprs = [Assign "x" (StrLit ""), If (Var "x") [Assign "y" (IntLit 42)] [Assign "y" (IntLit 0)]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal ""), ("y", IntVal 0)])
        
        it "should consider a non-empty string truthy" $ do
            let exprs = [Assign "x" (StrLit "hello"), If (Var "x") [Assign "y" (IntLit 42)] [Assign "y" (IntLit 0)]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal "hello"), ("y", IntVal 42)])
        
        it "should evaluate if statement with false condition" $ do
            let exprs = [Assign "x" (IntLit 0), If (Var "x") [Assign "y" (IntLit 42)] [Assign "y" (IntLit 0)]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 0), ("y", IntVal 0)])

        it "should evaluate if statement with multiple statements" $ do 
            let exprs = [Assign "x" (IntLit 1), If (Var "x") [Assign "y" (IntLit 42), Assign "z" (IntLit 43)] [Assign "y" (IntLit 0), Assign "z" (IntLit 0)]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 1), ("y", IntVal 42), ("z", IntVal 43)])

    describe "Value to string" $ do
        it "should correctly format an integer" $ do
            let exprs = IntVal 42
            let result = valueToString exprs

            result `shouldBe` "42"

        it "should correctly format a string" $ do
            let exprs = StrVal "hello"
            let result = valueToString exprs

            result `shouldBe` "\"hello\""

        it "should correctly format boolean true" $ do
            let exprs = BoolVal True
            let result = valueToString exprs

            result `shouldBe` "true"
        
        it "should correctly format boolean false" $ do
            let exprs = BoolVal False
            let result = valueToString exprs

            result `shouldBe` "false"
        
        it "should correctly format a list" $ do
            let exprs = ListVal [IntVal 1, IntVal 2, IntVal 3]
            let result = valueToString exprs

            result `shouldBe` "[1, 2, 3]"
        
        it "should correctly format a list with strings" $ do
            let exprs = ListVal [StrVal "hello", StrVal "world"]
            let result = valueToString exprs

            result `shouldBe` "[\"hello\", \"world\"]"
        
        it "should correctly format a list with mixed types" $ do
            let exprs = ListVal [IntVal 1, StrVal "hello", BoolVal True]
            let result = valueToString exprs

            result `shouldBe` "[1, \"hello\", true]"
        
        it "should be able to show a function" $ do
            let exprs = FuncVal ["x", "y"] [Add (Var "x") (Var "y")] Map.empty
            let result = valueToString exprs

            result `shouldBe` "<function>"

        it "should be able to show null" $ do
            let exprs = NullVal
            let result = valueToString exprs

            result `shouldBe` "null"

    describe "Print statement" $ do
      it "should evaluate print statement" $ do
          let exprs = [Print (IntLit 42)]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.empty)
    
    describe "Comparison operators" $ do
      it "should evaluate equality operator (==) to true if both items are equal for ints" $ do
          let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 42), Assign "z" (Eq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 42), ("z", BoolVal True)])

      it "should evaluate equality operator (==) to false if both items are not equal for ints" $ do
          let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 43), Assign "z" (Eq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 43), ("z", BoolVal False)])

      it "should evaluate equality operator (==) to true if both items are equal for strings" $ do
          let exprs = [Assign "x" (StrLit "hello"), Assign "y" (StrLit "hello"), Assign "z" (Eq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal "hello"), ("y", StrVal "hello"), ("z", BoolVal True)])
      
      it "should evaluate equality operator (==) to false if both items are not equal for strings" $ do
          let exprs = [Assign "x" (StrLit "hello"), Assign "y" (StrLit "world"), Assign "z" (Eq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal "hello"), ("y", StrVal "world"), ("z", BoolVal False)])

      it "should evaluate equality operator (==) to true if both items are equal for booleans" $ do
          let exprs = [Assign "x" (BoolLit True), Assign "y" (BoolLit True), Assign "z" (Eq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True), ("y", BoolVal True), ("z", BoolVal True)])
      
      it "should evaluate equality operator (==) to false if both items are not equal for booleans" $ do
          let exprs = [Assign "x" (BoolLit True), Assign "y" (BoolLit False), Assign "z" (Eq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True), ("y", BoolVal False), ("z", BoolVal False)])

      it "should evaluate inequality operator (!=) to true if both items are not equal for ints" $ do
          let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 43), Assign "z" (Neq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 43), ("z", BoolVal True)])
      
      it "should evaluate inequality operator (!=) to false if both items are equal for ints" $ do
          let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 42), Assign "z" (Neq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 42), ("z", BoolVal False)])

      it "should evaluate inequality operator (!=) to true if both items are not equal for strings" $ do
          let exprs = [Assign "x" (StrLit "hello"), Assign "y" (StrLit "world"), Assign "z" (Neq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal "hello"), ("y", StrVal "world"), ("z", BoolVal True)])
      
      it "should evaluate inequality operator (!=) to false if both items are equal for strings" $ do
          let exprs = [Assign "x" (StrLit "hello"), Assign "y" (StrLit "hello"), Assign "z" (Neq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", StrVal "hello"), ("y", StrVal "hello"), ("z", BoolVal False)])

      it "should evaluate inequality operator (!=) to false if both items are equal for booleans" $ do
          let exprs = [Assign "x" (BoolLit True), Assign "y" (BoolLit True), Assign "z" (Neq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True), ("y", BoolVal True), ("z", BoolVal False)])

      it "should evaluate inequality operator (!=) to true if both items are not equal for booleans" $ do
          let exprs = [Assign "x" (BoolLit True), Assign "y" (BoolLit False), Assign "z" (Neq (Var "x") (Var "y"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True), ("y", BoolVal False), ("z", BoolVal True)])

      it "should evaluate less than operator (<) to true if left item is less than right item for ints" $ do
        let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 43), Assign "z" (Lt (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 43), ("z", BoolVal True)])

      it "should evaluate less than operator (<) to false if left item is not less than right item for ints" $ do
        let exprs = [Assign "x" (IntLit 43), Assign "y" (IntLit 42), Assign "z" (Lt (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 43), ("y", IntVal 42), ("z", BoolVal False)])

      it "should evaluate greater than operator (>) to true if left item is greater than right item for ints" $ do
        let exprs = [Assign "x" (IntLit 43), Assign "y" (IntLit 42), Assign "z" (Gt (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 43), ("y", IntVal 42), ("z", BoolVal True)])

      it "should evaluate greater than operator (>) to false if left item is not greater than right item for ints" $ do
        let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 43), Assign "z" (Gt (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 43), ("z", BoolVal False)])

      it "should evaluate greater than or equal operator (>=) to true if left item is greater than or equal to right item for ints" $ do
        let exprs = [Assign "x" (IntLit 43), Assign "y" (IntLit 42), Assign "z" (Ge (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 43), ("y", IntVal 42), ("z", BoolVal True)])

      it "should evaluate greater than or equal operator (>=) to true if left item is equal to right item for ints" $ do
        let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 42), Assign "z" (Ge (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 42), ("z", BoolVal True)])

      it "should evaluate greater than or equal operator (>=) to false if left item is not greater than or equal to right item for ints" $ do
        let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 43), Assign "z" (Ge (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 43), ("z", BoolVal False)])

      it "should evaluate less than or equal operator (<=) to true if left item is less than or equal to right item for ints" $ do
        let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 43), Assign "z" (Le (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 43), ("z", BoolVal True)])

      it "should evaluate less than or equal operator (<=) to true if left item is equal to right item for ints" $ do
        let exprs = [Assign "x" (IntLit 42), Assign "y" (IntLit 42), Assign "z" (Le (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42), ("y", IntVal 42), ("z", BoolVal True)])

      it "should evaluate less than or equal operator (<=) to false if left item is not less than or equal to right item for ints" $ do
        let exprs = [Assign "x" (IntLit 43), Assign "y" (IntLit 42), Assign "z" (Le (Var "x") (Var "y"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 43), ("y", IntVal 42), ("z", BoolVal False)])

      it "should throw error when using the equal operator (==) on two different types" $ do
        let exprs = [Eq (IntLit 42) (StrLit "hello")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft
      
      it "should throw error when using the equal operator (!=) on two different types" $ do
        let exprs = [Neq (IntLit 42) (StrLit "hello")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft

      it "should throw error when using the less than operator (<) on two different types" $ do
        let exprs = [Lt (IntLit 42) (StrLit "hello")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft
      
      it "should throw error when using the greater than operator (>) on two different types" $ do
        let exprs = [Gt (IntLit 42) (StrLit "hello")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft
      
      it "should throw error when using the less or equal operator (<=) on two different types" $ do
        let exprs = [Le (IntLit 42) (StrLit "hello")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft
      
      it "should throw error when using the greater or equal operator (>=) on two different types" $ do
        let exprs = [Ge (IntLit 42) (StrLit "hello")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft
      
      it "should throw error when using the less than operator (<) on strings" $ do
        let exprs = [Lt (StrLit "hello") (StrLit "world")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft
      
      it "should throw error when using the greater than operator (>) on strings" $ do
        let exprs = [Gt (StrLit "hello") (StrLit "world")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft
      
      it "should throw error when using the less or equal operator (<=) on strings" $ do
        let exprs = [Le (StrLit "hello") (StrLit "world")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft
      
      it "should throw error when using the greater or equal operator (>=) on strings" $ do
        let exprs = [Ge (StrLit "hello") (StrLit "world")]
        result <- runInterpreter Map.empty exprs
        result `shouldSatisfy` isLeft

    describe "List operations" $ do
      it "should evaluate list literal" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3])]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 1, IntVal 2, IntVal 3])])

      it "should evaluate list access" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListAccess (Var "x") (IntLit 0))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 1, IntVal 2, IntVal 3]), ("y", IntVal 1)])
      
      it "should evaluate list append" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListAppend (Var "x") (IntLit 4))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 1, IntVal 2, IntVal 3]), ("y", ListVal [IntVal 1, IntVal 2, IntVal 3, IntVal 4])])
      
      it "should evaluate list remove" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListRemove (Var "x") (IntLit 1))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 1, IntVal 2, IntVal 3]), ("y", ListVal [IntVal 1, IntVal 3])])
      
      it "should evaluate list pop" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListPop (Var "x") (IntLit 1))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 1, IntVal 3]), ("y", IntVal 2)])
      
      it "should evaluate list add" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListAdd (Var "x") (IntLit 0) (IntLit 4))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 1, IntVal 2, IntVal 3]), ("y", ListVal [ IntVal 4, IntVal 1, IntVal 2, IntVal 3])])
      
      it "should throw error when trying to access an index that is out of bounds" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListAccess (Var "x") (IntLit 3))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Index out of bounds"
      
      it "should throw error when trying to remove an index that is out of bounds" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListRemove (Var "x") (IntLit 3))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Index out of bounds"
      
      it "should throw error when trying to pop an index that is out of bounds" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListPop (Var "x") (IntLit 3))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Index out of bounds"
      
      it "should throw error when trying to add an index that is out of bounds" $ do
          let exprs = [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3]), Assign "y" (ListAdd (Var "x") (IntLit 4) (IntLit 4))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Index out of bounds"

    describe "Function Definitions" $ do
      it "should evaluate a function definition" $ do
          let exprs = [FuncDef "add" ["x", "y"] [Return (Add (Var "x") (Var "y"))]]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("add", FuncVal ["x", "y"] [Return (Add (Var "x") (Var "y"))] Map.empty)])

    describe "Function Calls" $ do
      it "should evaluate a function call" $ do
          let exprs = [FuncDef "add" ["x", "y"] [Return (Add (Var "x") (Var "y"))], FuncCall "add" [IntLit 1, IntLit 2]]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("add", FuncVal ["x", "y"] [Return (Add (Var "x") (Var "y"))] Map.empty)])

      it "should allow to assign the result of a function call" $ do
          let exprs = [FuncDef "add" ["x", "y"] [Return (Add (Var "x") (Var "y"))], Assign "z" (FuncCall "add" [IntLit 1, IntLit 2])]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("add", FuncVal ["x", "y"] [Return (Add (Var "x") (Var "y"))] Map.empty), ("z", IntVal 3)])

    describe "Loop controls" $ do
        describe "haltLoop statement" $ do
            it "should evaluate a haltLoop statement" $ do
                let exprs = [WhileLoop (BoolLit True) [Assign "x" (IntLit 1), Break, Assign "y" (IntLit 2)]]
                result <- runInterpreter Map.empty exprs
                result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 1)])
            
            it "should evaluate a haltLoop statement inside a nested loop" $ do
                let exprs = [WhileLoop (BoolLit True) [Assign "x" (IntLit 1), WhileLoop (BoolLit True) [Assign "y" (IntLit 2), Break, Assign "z" (IntLit 3)], Break, Assign "w" (IntLit 4)]]
                result <- runInterpreter Map.empty exprs
                result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 1), ("y", IntVal 2)])

        describe "next statements" $ do
            it "should evaluate a next statement" $ do
                let exprs = [Assign "x" (IntLit 0), WhileLoop (Lt (Var "x") (IntLit 5)) [Assign "x" (Add (Var "x") (IntLit 1)), Next]]
                result <- runInterpreter Map.empty exprs
                result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 5)])

    describe "Range" $ do
        it "should evaluate a range statement with a default step of 1" $ do
            let exprs = [Assign "x" (ListRange (IntLit 1) (IntLit 5) Nothing)]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 1, IntVal 2, IntVal 3, IntVal 4, IntVal 5])])

        it "should evaluate a range statement with a step of 2" $ do
            let exprs = [Assign "x" (ListRange (IntLit 1) (IntLit 5) (Just (IntLit 2)))]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 1, IntVal 3, IntVal 5])])

        it "should evaluate a range statement with a negative step" $ do
            let exprs = [Assign "x" (ListRange (IntLit 5) (IntLit 1) (Just (IntLit (-1))))]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ListVal [IntVal 5, IntVal 4, IntVal 3, IntVal 2, IntVal 1])])

    describe "Objects" $ do
        it "should evaluate an object literal" $ do
            let exprs = [Assign "x" (ObjectDef [("a", IntLit 1), ("b", IntLit 2)])]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ObjectVal [("a", IntVal 1), ("b", IntVal 2)])])
        
        it "should evaluate nested object literals" $ do
            let exprs = [Assign "x" (ObjectDef [("a", IntLit 1), ("b", ObjectDef [("c", IntLit 2)])])]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ObjectVal [("a", IntVal 1), ("b", ObjectVal [("c", IntVal 2)])])])

        it "should evaluate an object access" $ do
            let exprs = [Assign "x" (ObjectDef [("a", IntLit 1), ("b", IntLit 2)]), Assign "y" (ObjectAccess (Var "x") "a")]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ObjectVal [("a", IntVal 1), ("b", IntVal 2)]), ("y", IntVal 1)])

        it "should evaluate an object access with nested objects" $ do
            let exprs = [Assign "x" (ObjectDef [("a", IntLit 1), ("b", ObjectDef [("c", IntLit 2)])]), Assign "y" (ObjectAccess (ObjectAccess (Var "x") "b") "c")]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ObjectVal [("a", IntVal 1), ("b", ObjectVal [("c", IntVal 2)])]), ("y", IntVal 2)])

        it "should evaluate object set" $ do
            let exprs = [Assign "x" (ObjectDef [("a", IntLit 1), ("b", IntLit 2)]), ObjectSet (Var "x") "a" (IntLit 3)]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ObjectVal [("b",IntVal 2),("a",IntVal 3)])])
        
        it "should evaluate object set with nested objects" $ do
            let exprs = [Assign "x" (ObjectDef [("a", IntLit 1), ("d", IntLit 3), ("b", ObjectDef [("c", IntLit 2)])]), ObjectSet (Var "x") "b.c" (IntLit 3)]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ObjectVal [("b",ObjectVal [("c",IntVal 3)]),("a",IntVal 1),("d",IntVal 3)])])

        it "should evaluate setting an object property to another object" $ do
            let exprs = [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30)]), ObjectSet (Var "x") "address" (ObjectDef [("city", StrLit "New York"), ("country", StrLit "USA")])]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ObjectVal [("name",StrVal "John"),("age",IntVal 30),("address",ObjectVal [("city",StrVal "New York"),("country",StrVal "USA")])])])
        
        it "should evaluate an object boolean property" $ do
            let exprs = [Assign "x" (ObjectDef [("a", BoolLit True), ("b", BoolLit False)]), Assign "y" (ObjectAccess (Var "x") "a")]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", ObjectVal [("a", BoolVal True), ("b", BoolVal False)]), ("y", BoolVal True)])


    describe "Logical operations" $ do
      it "should evaluate logical and operator (and)" $ do
          let exprs = [Assign "x" (LogicAnd (BoolLit True) (BoolLit False))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal False)])
      
      it "should evaluate logical and operator (and) with strings" $ do
          let exprs = [Assign "x" (LogicAnd (StrLit "hello") (StrLit "world"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True)])
          
      it "should evaluate logical and operator (and) with ints" $ do
          let exprs = [Assign "x" (LogicAnd (IntLit 1) (IntLit 2))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True)])

      it "should evaluate logical or operator (or)" $ do
          let exprs = [Assign "x" (LogicOr (BoolLit True) (BoolLit False))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True)])

      it "should evaluate logical or operator (or) with strings" $ do
          let exprs = [Assign "x" (LogicOr (StrLit "hello") (StrLit "world"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True)])        
      
      it "should evaluate logical or operator (or) with integers" $ do
          let exprs = [Assign "x" (LogicOr (IntLit 0) (IntLit 5))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True)])        

      it "should evaluate logical not operator (not)" $ do
          let exprs = [Assign "x" (LogicNot (BoolLit True))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal False)])
      
      it "should evaluate logical not operator (not) with strings" $ do
          let exprs = [Assign "x" (LogicNot (StrLit "hello"))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal False)])

      it "should evaluate logical not operator (not) with integers" $ do
          let exprs = [Assign "x" (LogicNot (IntLit 5))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal False)])


      it "should throw error when using the logical and operator (and) on two different types" $ do
          let exprs = [LogicAnd (BoolLit True) (IntLit 42)]
          result <- runInterpreter Map.empty exprs
          result `shouldSatisfy` isLeft
      
      it "should throw error when using the logical or operator (or) on two different types" $ do
          let exprs = [LogicOr (BoolLit True) (IntLit 42)]
          result <- runInterpreter Map.empty exprs
          result `shouldSatisfy` isLeft

      it "should give the correct truth table for the logical and operator (and)" $ do
          let exprs = [Assign "x" (LogicAnd (BoolLit True) (BoolLit True)), Assign "y" (LogicAnd (BoolLit True) (BoolLit False)), Assign "z" (LogicAnd (BoolLit False) (BoolLit True)), Assign "w" (LogicAnd (BoolLit False) (BoolLit False))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True), ("y", BoolVal False), ("z", BoolVal False), ("w", BoolVal False)])

      it "should give the correct truth table for the logical or operator (or)" $ do
          let exprs = [Assign "x" (LogicOr (BoolLit True) (BoolLit True)), Assign "y" (LogicOr (BoolLit True) (BoolLit False)), Assign "z" (LogicOr (BoolLit False) (BoolLit True)), Assign "w" (LogicOr (BoolLit False) (BoolLit False))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal True), ("y", BoolVal True), ("z", BoolVal True), ("w", BoolVal False)])
      
      it "should give the correct truth table for the logical not operator (not)" $ do
          let exprs = [Assign "x" (LogicNot (BoolLit True)), Assign "y" (LogicNot (BoolLit False))]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", BoolVal False), ("y", BoolVal True)])

    describe "Import module" $ do
      it "should import a module if it exists" $ do
          let exprs = [ImportModule "./test/testModule"]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList([("x", BoolVal True)]))
      it "should throw an error if the module does not exist" $ do
          let exprs = [ImportModule "./test/nonExistentModule"]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Left "Module ./test/nonExistentModule does not exist"

    describe "Value to string" $ do
        it "should correctly format an integer" $ do
            let exprs = IntVal 42
            let result = valueToString exprs
    
            result `shouldBe` "42"
        
        it "should correctly format a string" $ do
            let exprs = StrVal "hello"
            let result = valueToString exprs
    
            result `shouldBe` "\"hello\""
    
        it "should correctly format boolean true" $ do
            let exprs = BoolVal True
            let result = valueToString exprs
    
            result `shouldBe` "true"
        
        it "should correctly format boolean false" $ do
            let exprs = BoolVal False
            let result = valueToString exprs
    
            result `shouldBe` "false"
        
        it "should correctly format a list" $ do
            let exprs = ListVal [IntVal 1, IntVal 2, IntVal 3]
            let result = valueToString exprs
    
            result `shouldBe` "[1, 2, 3]"
        
        it "should correctly format a list with strings" $ do
            let exprs = ListVal [StrVal "hello", StrVal "world"]
            let result = valueToString exprs
    
            result `shouldBe` "[\"hello\", \"world\"]"
        
        it "should correctly format a list with mixed types" $ do
            let exprs = ListVal [IntVal 1, StrVal "hello", BoolVal True]
            let result = valueToString exprs
    
            result `shouldBe` "[1, \"hello\", true]"
        
        it "should be able to show a function" $ do
            let exprs = FuncVal ["x", "y"] [Add (Var "x") (Var "y")] Map.empty
            let result = valueToString exprs
    
            result `shouldBe` "<function>"
    
        it "should be able to show null" $ do
            let exprs = NullVal
            let result = valueToString exprs
    
            result `shouldBe` "null"
        
        it "should be able to show an object" $ do
            let exprs = ObjectVal [("a", IntVal 1), ("b", IntVal 2)]
            let result = valueToString exprs
    
            result `shouldBe` "{a: 1, b: 2}"

    describe "While loops" $ do
        it "should evaluate a while loop" $ do
            let exprs = [Assign "x" (IntLit 0), WhileLoop (Lt (Var "x") (IntLit 2)) [Assign "x" (Add (Var "x") (IntLit 1))]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 2)])
        
        it "should evaluate a while loop with multiple statements" $ do
            let exprs = [Assign "x" (IntLit 0), WhileLoop (Lt (Var "x") (IntLit 2)) [Assign "x" (Add (Var "x") (IntLit 1)), Assign "y" (IntLit 42)]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 2), ("y", IntVal 42)])
        
        it "should evaluate a do while loop" $ do
            let exprs = [Assign "x" (IntLit 0), DoWhileLoop (Lt (Var "x") (IntLit 2)) [Assign "x" (Add (Var "x") (IntLit 1))]]
            result <- runInterpreter Map.empty exprs
            result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 2)])

    describe "For loop" $ do
      it "should evaluate a for loop" $ do
          let exprs = [ForLoop (Assign "x" (IntLit 0)) (Lt (Var "x") (IntLit 2)) (Assign "x" (Add (Var "x") (IntLit 1))) []]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 2)])
    