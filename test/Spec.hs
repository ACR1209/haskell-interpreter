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
      
  describe "Interpreter" $ do
    it "should evaluate integer assignment" $ do
        let exprs = [Assign "x" (IntLit 42)]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Left 42)])

    it "should evaluate string assignment" $ do
        let exprs = [Assign "x" (StrLit "hello")]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Right "hello")])

    it "should evaluate addition of integers" $ do
        let exprs = [Assign "x" (Add (IntLit 42) (IntLit 10))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Left 52)])

    it "should evaluate string concatenation" $ do
        let exprs = [Assign "x" (Add (StrLit "hello") (StrLit "world"))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Right "helloworld")])

    it "should evaluate subtraction of integers" $ do
        let exprs = [Assign "x" (Sub (IntLit 42) (IntLit 10))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Left 32)])

    it "should evaluate multiplication of integers" $ do
        let exprs = [Assign "x" (Mul (IntLit 42) (IntLit 10))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Left 420)])

    it "should evaluate division of integers" $ do
        let exprs = [Assign "x" (Div (IntLit 42) (IntLit 10))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Left 4)])

    it "should handle division by zero" $ do
        let exprs = [Assign "x" (Div (IntLit 42) (IntLit 0))]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Left "Division by zero"

    it "should evaluate if statement with true condition" $ do
        let exprs = [Assign "x" (IntLit 1), If (Var "x") [Assign "y" (IntLit 42)] [Assign "y" (IntLit 0)]]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Left 1), ("y", Left 42)])

    it "should evaluate if statement with false condition" $ do
        let exprs = [Assign "x" (IntLit 0), If (Var "x") [Assign "y" (IntLit 42)] [Assign "y" (IntLit 0)]]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Left 0), ("y", Left 0)])

    it "should evaluate print statement" $ do
        let exprs = [Print (IntLit 42)]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.empty)
    
    it "should recover string variables" $ do
        let exprs = [Assign "x" (StrLit "hello"), Var "x"]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Right "hello")])

    it "should recover integer variables" $ do
        let exprs = [Assign "x" (IntLit 42), Var "x"]
        result <- runInterpreter Map.empty exprs
        result `shouldBe` Right (0, Map.fromList [("x", Left 42)])

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
    
    