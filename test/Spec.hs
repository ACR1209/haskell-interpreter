import Test.Hspec
import SimpleParser
import Interpreter
import Data.Either (isLeft)
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "SimpleParser" $ do
    describe "Basic operations" $ do
      it "should parse variable assigment" $ do
        parseProgram "x = 42" `shouldBe` Right [Assign "x" (IntLit 42)]
      it "should parse addition" $ do
        parseProgram "x = 42 + 10" `shouldBe` Right [Assign "x" (Add (IntLit 42) (IntLit 10))]
      it "should parse subtraction" $ do
        parseProgram "x = 42 - 10" `shouldBe` Right [Assign "x" (Sub (IntLit 42) (IntLit 10))]
      it "should parse multiplication" $ do
        parseProgram "x = 42 * 10" `shouldBe` Right [Assign "x" (Mul (IntLit 42) (IntLit 10))]
      it "should parse division" $ do
        parseProgram "x = 42 / 10" `shouldBe` Right [Assign "x" (Div (IntLit 42) (IntLit 10))]
      it "should parse string concatenation" $ do
        parseProgram "x = \"hello\" + \"world\"" `shouldBe` Right [Assign "x" (Add (StrLit "hello") (StrLit "world"))]
      it "should parse expression with parentheses" $ do
        parseProgram "x = (42 + 10) * 2" `shouldBe` Right [Assign "x" (Mul (Add (IntLit 42) (IntLit 10)) (IntLit 2))]
      it "should parse the true boolean" $ do
        parseProgram "x = true" `shouldBe` Right [Assign "x" (BoolLit True)]
      it "should parse the false boolean" $ do
        parseProgram "x = false" `shouldBe` Right [Assign "x" (BoolLit False)]

    describe "If statement" $ do
      it "should parse if statement" $ do
        parseProgram "if x { print x } else { print \"no x\" }" `shouldBe` Right [If (Var "x") [Print (Var "x")] [Print (StrLit "no x")]]
      it "should parse if statement with multiple statements" $ do
        parseProgram "if x { print x print x } else { print \"no x\" print \"no x\" }" `shouldBe` Right [If (Var "x") [Print (Var "x"), Print (Var "x")] [Print (StrLit "no x"), Print (StrLit "no x")]]
      it "should parse if statement with nested if" $ do
        parseProgram "if x { if y { print x } else { print y } } else { print \"no x\" }" `shouldBe` Right [If (Var "x") [If (Var "y") [Print (Var "x")] [Print (Var "y")]] [Print (StrLit "no x")]]
      it "should parse if statement with no else" $ do
        parseProgram "if x { print x }" `shouldBe` Right [If (Var "x") [Print (Var "x")] []]
    
    describe "Print statement" $ do
      it "should parse print statement" $ do
        parseProgram "print x" `shouldBe` Right [Print (Var "x")]
      it "should give error when print statement used without parameter" $ do
        parseProgram "print" `shouldSatisfy` isLeft 

    describe "Comparison operators" $ do
      it "should parse equality operator (==) for integers" $ do
        parseProgram "x = 42 == 42" `shouldBe` Right [Assign "x" (Eq (IntLit 42) (IntLit 42))]
      it "should parse equality operator (==) for strings" $ do
        parseProgram "x = \"hello\" == \"hello\"" `shouldBe` Right [Assign "x" (Eq (StrLit "hello") (StrLit "hello"))]
      it "should parse inequality operator (!=) for integers" $ do
        parseProgram "x = 42 != 43" `shouldBe` Right [Assign "x" (Neq (IntLit 42) (IntLit 43))]
      it "should parse inequality operator (!=) for strings" $ do
        parseProgram "x = \"hello\" != \"world\"" `shouldBe` Right [Assign "x" (Neq (StrLit "hello") (StrLit "world"))]
      it "should parse less than operator (<) for integers" $ do
        parseProgram "x = 42 < 43" `shouldBe` Right [Assign "x" (Lt (IntLit 42) (IntLit 43))]
      it "should parse greater than operator (>) for integers" $ do
        parseProgram "x = 43 > 42" `shouldBe` Right [Assign "x" (Gt (IntLit 43) (IntLit 42))]
      it "should parse less than or equal operator (<=) for integers" $ do
        parseProgram "x = 42 <= 43" `shouldBe` Right [Assign "x" (Le (IntLit 42) (IntLit 43))]
      it "should parse greater than or equal operator (>=) for integers" $ do
        parseProgram "x = 43 >= 42" `shouldBe` Right [Assign "x" (Ge (IntLit 43) (IntLit 42))]

    describe "List operations" $ do
      it "should parse list literal" $ do
        parseProgram "x = [1, 2, 3]" `shouldBe` Right [Assign "x" (ListLit [IntLit 1, IntLit 2, IntLit 3])]
      it "should parse list access" $ do
        parseProgram "x = [1, 2, 3][0]" `shouldBe` Right [Assign "x" (ListAccess (ListLit [IntLit 1, IntLit 2, IntLit 3]) (IntLit 0))]
      it "should parse list append" $ do
        parseProgram "x = [1, 2, 3] << 4" `shouldBe` Right [Assign "x" (ListAppend (ListLit [IntLit 1, IntLit 2, IntLit 3]) (IntLit 4))]
      it "should parse list remove" $ do
        parseProgram "x = [1, 2, 3] >> 2" `shouldBe` Right [Assign "x" (ListRemove (ListLit [IntLit 1, IntLit 2, IntLit 3]) (IntLit 2))]
      it "should parse list pop" $ do
        parseProgram "x = [1, 2, 3] =>> 0" `shouldBe` Right [Assign "x" (ListPop (ListLit [IntLit 1, IntLit 2, IntLit 3]) (IntLit 0))]
      it "should parse list add" $ do
        parseProgram "x = [1, 2, 3] << 0 <<= 4" `shouldBe` Right [Assign "x" (ListAdd (ListLit [IntLit 1, IntLit 2, IntLit 3]) (IntLit 0) (IntLit 4))]

    describe "Comments" $ do
      it "should parse single line comment" $ do
        parseProgram "# This is a comment" `shouldBe` Right [Comment "This is a comment"]
      it "should parse multi line comment" $ do
        parseProgram "/* This is a comment */" `shouldBe` Right [MultiLineComment "This is a comment"]
      it "should parse multi line comment with new lines" $ do
        parseProgram "/* This is a \n comment */" `shouldBe` Right [MultiLineComment "This is a \n comment"]
      it "should parse a comment with a # in it" $ do
        parseProgram "# This is a comment with a # in it" `shouldBe` Right [Comment "This is a comment with a # in it"]
      it "should parse a comment with a /* in it" $ do
        parseProgram "# This is a comment with a /* in it" `shouldBe` Right [Comment "This is a comment with a /* in it"]
      it "should parse a comment with a */ in it" $ do
        parseProgram "# This is a comment with a */ in it" `shouldBe` Right [Comment "This is a comment with a */ in it"]        
      it "should correctly parse inline comments on the side of a statement" $ do
        parseProgram "x = 42 # This is a comment" `shouldBe` Right [Assign "x" (IntLit 42), Comment "This is a comment"]

    describe "For loop" $ do
      it "should parse a for loop" $ do
        parseProgram "for (x = 0; x < 2; x = x + 1) {}" `shouldBe` Right [ForLoop (Assign "x" (IntLit 0)) (Lt (Var "x") (IntLit 2)) (Assign "x" (Add (Var "x") (IntLit 1))) []]

  describe "Interpreter" $ do
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

    describe "Comments" $ do
      it "should ignore single line comments" $ do
          let exprs = [Comment "This is a comment", Assign "x" (IntLit 42)]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42)])
      
      it "should ignore multi line comments" $ do
          let exprs = [MultiLineComment "This is a comment", Assign "x" (IntLit 42)]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 42)])

    describe "For loop" $ do
      it "should evaluate a for loop" $ do
          let exprs = [ForLoop (Assign "x" (IntLit 0)) (Lt (Var "x") (IntLit 2)) (Assign "x" (Add (Var "x") (IntLit 1))) []]
          result <- runInterpreter Map.empty exprs
          result `shouldBe` Right (IntVal 0, Map.fromList [("x", IntVal 2)])
    