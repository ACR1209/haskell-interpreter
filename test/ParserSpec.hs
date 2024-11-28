module ParserSpec (spec) where

import Test.Hspec
import SimpleParser
import Data.Either (isLeft)

spec :: Spec
spec = context "SimpleParser" $ do
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

    describe "Range" $ do
      it "should parse range" $ do
        parseProgram "x = 1..10" `shouldBe` Right [Assign "x" (ListRange (IntLit 1) (IntLit 10) (Nothing))]
      it "should parse range with step" $ do
        parseProgram "x = 1..10 stepping 2" `shouldBe` Right [Assign "x" (ListRange (IntLit 1) (IntLit 10) (Just (IntLit 2)))]

    describe "Comments" $ do
      it "should parse single line comment" $ do
        parseProgram "# This is a comment" `shouldBe` Right []
      it "should parse multi line comment" $ do
        parseProgram "/* This is a comment */" `shouldBe` Right []
      it "should parse multi line comment with new lines" $ do
        parseProgram "/* This is a \n comment */" `shouldBe` Right []
      it "should parse a comment with a # in it" $ do
        parseProgram "# This is a comment with a # in it" `shouldBe` Right []
      it "should parse a comment with a /* in it" $ do
        parseProgram "# This is a comment with a /* in it" `shouldBe` Right []
      it "should parse a comment with a */ in it" $ do
        parseProgram "# This is a comment with a */ in it" `shouldBe` Right []        
      it "should correctly parse inline comments on the side of a statement" $ do
        parseProgram "x = 42 # This is a comment" `shouldBe` Right [Assign "x" (IntLit 42)]

    describe "Loop controls" $ do
      it "should parse next statement inside a loop" $ do
        parseProgram "loop (true) { next }" `shouldBe` Right [WhileLoop (BoolLit True) [Next]]

      it "should parse next if statement inside a loop" $ do
        parseProgram "loop (true) { next if true }" `shouldBe` Right [WhileLoop (BoolLit True) [If (BoolLit True) [Next] []]]

      it "should parse next unless statement inside a loop" $ do
        parseProgram "loop (true) { next unless true }" `shouldBe` Right [WhileLoop (BoolLit True) [If (BoolLit True) [] [Next]]]

      it "should parse haltLoop statement inside a loop" $ do
        parseProgram "loop (true) { haltLoop }" `shouldBe` Right [WhileLoop (BoolLit True) [Break]]

      it "should throw an error if next statement is used outside a loop" $ do
        parseProgram "next" `shouldSatisfy` isLeft

      it "should throw an error if next if statement is used outside a loop" $ do
        parseProgram "next if true" `shouldSatisfy` isLeft
      
      it "should throw an error if next unless statement is used outside a loop" $ do
        parseProgram "next unless true" `shouldSatisfy` isLeft

      it "should throw an error if haltLoop statement is used outside a loop" $ do
        parseProgram "haltLoop" `shouldSatisfy` isLeft

    describe "Function definition" $ do
      it "should parse a function definition" $ do
        parseProgram "def add(x, y) { return x + y }" `shouldBe` Right [FuncDef "add" ["x", "y"] [Return (Add (Var "x") (Var "y"))]]

      it "should fail if the function definition is missing the return keyword" $ do
        parseProgram "def add(x, y) { x + y }" `shouldSatisfy` isLeft
      
      it "should fail if the return keyword is used outside a function" $ do
        parseProgram "return 42" `shouldSatisfy` isLeft

    describe "Function call" $ do
      it "should parse a function call as a statement" $ do
        parseProgram "add(1, 2)" `shouldBe` Right [FuncCall "add" [IntLit 1, IntLit 2]]
      it "should parse a function call as an expression" $ do
        parseProgram "x = add(1, 2)" `shouldBe` Right [Assign "x" (FuncCall "add" [IntLit 1, IntLit 2])]

    describe "Logical operations" $ do
      it "should be able to parse the logical and operator (and)" $ do
        parseProgram "x = true and false" `shouldBe` Right [Assign "x" (LogicAnd (BoolLit True) (BoolLit False))]
      it "should be able to parse the logical or operator (or)" $ do
        parseProgram "x = true or false" `shouldBe` Right [Assign "x" (LogicOr (BoolLit True) (BoolLit False))]
      it "should be able to parse the logical not operator (not)" $ do
        parseProgram "x = not true" `shouldBe` Right [Assign "x" (LogicNot (BoolLit True))]

    describe "For loop" $ do
      it "should parse a for loop" $ do
        parseProgram "for (x = 0; x < 2; x = x + 1) {}" `shouldBe` Right [ForLoop (Assign "x" (IntLit 0)) (Lt (Var "x") (IntLit 2)) (Assign "x" (Add (Var "x") (IntLit 1))) []]

    describe "While loop" $ do
      it "should parse a while loop" $ do
        parseProgram "loop (x < 2) {  }" `shouldBe` Right [WhileLoop (Lt (Var "x") (IntLit 2)) []]

      it "should parse a do while loop" $ do
        parseProgram "do {} loop (x < 2)" `shouldBe` Right [DoWhileLoop (Lt (Var "x") (IntLit 2)) []]

    describe "Import module" $ do
      it "should parse an import module statement" $ do
        parseProgram "import module" `shouldBe` Right [ImportModule "module"]

 