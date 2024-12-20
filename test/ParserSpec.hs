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

    describe "Objects" $ do
      it "should parse object literal" $ do
        parseProgram "x = {name: \"John\", age: 30}" `shouldBe` Right [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30)])]

      it "should parse object literal with empty object" $ do
        parseProgram "x = {}" `shouldBe` Right [Assign "x" (ObjectDef [])]

      it "should parse object literal with property keys as strings" $ do
        parseProgram "x = {\"name\": \"John\", \"age\": 30}" `shouldBe` Right [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30)])]

      it "should parse object literal with nested object" $ do
        parseProgram "x = {name: \"John\", age: 30, address: {city: \"New York\", country: \"USA\"}}" `shouldBe` Right [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30), ("address", ObjectDef [("city", StrLit "New York"), ("country", StrLit "USA")])])]

      it "should parse object access" $ do
        parseProgram "x = {name: \"John\", age: 30}\ny=x.name" `shouldBe` Right [Assign "x" (ObjectDef [("name",StrLit "John"),("age",IntLit 30)]),Assign "y" (ObjectAccess (Var "x") "name")]
    
      it "should parse object access with nested object" $ do
        parseProgram "x = {name: \"John\", age: 30, address: {city: \"New York\", country: \"USA\"}}\ny=x.address.city" `shouldBe` Right [Assign "x" (ObjectDef [("name",StrLit "John"),("age",IntLit 30),("address",ObjectDef [("city",StrLit "New York"),("country",StrLit "USA")])]),Assign "y" (ObjectAccess (ObjectAccess (Var "x") "address") "city")]

      it "should parse object and allow access directly" $ do
        parseProgram "x = {name: \"John\", age: 30}.name" `shouldBe` Right [Assign "x" (ObjectAccess (ObjectDef [("name",StrLit "John"),("age",IntLit 30)]) "name")]
      
      it "should parse anonymous object" $ do
        parseProgram "x = {\"name\": \"John\", \"age\": 30}" `shouldBe` Right [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30)])]

      it "should parse object set" $ do
        parseProgram "x = {name: \"John\", age: 30}\nx.name = \"Jane\"" `shouldBe` Right [Assign "x" (ObjectDef [("name",StrLit "John"),("age",IntLit 30)]),ObjectSet (Var "x") "name" (StrLit "Jane")]

      it "should parse object set with nested object" $ do
        parseProgram "x = {name: \"John\", age: 30, address: {city: \"New York\", country: \"USA\"}}\nx.address.city = \"Los Angeles\"" `shouldBe` Right [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30), ("address", ObjectDef [("city", StrLit "New York"), ("country", StrLit "USA")])]), ObjectSet (Var "x") "address.city" (StrLit "Los Angeles")]

      it "should parse object set by setting a property to an object" $ do
        parseProgram "x = {name: \"John\", age: 30}\nx.address = {city: \"New York\", country: \"USA\"}" `shouldBe` Right [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30)]), ObjectSet (Var "x") "address" (ObjectDef [("city", StrLit "New York"), ("country", StrLit "USA")])]

      it "should parse a boolean property" $ do
        parseProgram "x = {name: \"John\", age: 30, isStudent: true}" `shouldBe` Right [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30), ("isStudent", BoolLit True)])]

      it "should parse properly independent of the line breaks" $ do
        parseProgram "x = {\nname: \"John\", age: 30,\n isStudent: true\n}\n" `shouldBe` Right [Assign "x" (ObjectDef [("name", StrLit "John"), ("age", IntLit 30), ("isStudent", BoolLit True)])]

    describe "String interpolation" $ do
      it "should parse string interpolation" $ do
        parseProgram "x = f\"Hello, {name}\"" `shouldBe` Right [Assign "x" (StringInterpolation ["Hello, ", ""] [(Var "name")])]

      it "should parse multiple string interpolation" $ do
                parseProgram "x = f\"Hello, {name} {age}\"" `shouldBe` Right [Assign "x" (StringInterpolation ["Hello, ", " ", ""] [(Var "name"), (Var "age")])]


    describe "getInterpolationsFromString" $ do
      it "should get interpolations from a string" $ do
        getInterpolationsFromString "Hello, {name}" `shouldBe` ["name"]
    
      it "should get multiple interpolations from a string" $ do
        getInterpolationsFromString "Hello, {name}, {age}" `shouldBe` ["name", "age"]

      it "should get interpolations from a string with multiple interpolations in a row" $ do
        getInterpolationsFromString "Hello, {name}{age}" `shouldBe` ["name", "age"]

      it "should get interpolations from a string with multiple interpolations in a row and some text in between" $ do
        getInterpolationsFromString "Hello, {name} is {age} years old" `shouldBe` ["name", "age"]

    describe "parseInterpolations" $ do
      it "should parse expressions given certain interpolations" $ do
        parseInterpolations ["name", "true", "1+3"] `shouldBe` [Var "name", BoolLit True, Add (IntLit 1) (IntLit 3)]

      it "should work if no interpolation given" $ do
        parseInterpolations [] `shouldBe` []

    describe "splitAndRemoveInterpolations" $ do
      it "should remove interpolations of a string given the list of interpolations" $ do
        splitAndRemoveInterpolations "hello, {name}. You are {age} years old!" ["name", "age"] `shouldBe` ["hello, ", ". You are ", " years old!"]

      it "should contain a singlenton list of strings if no interpolation is present" $ do
        splitAndRemoveInterpolations "hello!" [] `shouldBe` ["hello!"]

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

 