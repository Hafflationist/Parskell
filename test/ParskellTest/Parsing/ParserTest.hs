module ParskellTest.Parsing.ParserTest where

import Data.Text
import Parskell.Parsing.Parser
import Parskell.ExpressionTree
import Parskell.Lexing.Tokens
import Test.Hspec
import Test.HUnit


parskellTest = do
    it "returns a simple expression tree +" $ do
        let tokens = [
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "1"}
                     ] -- 1+1
        let expressionTree = Parskell.ExpressionTree.Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree -" $ do
        let tokens = [
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "-"},
                        LiteralNumber {content = Data.Text.pack "1"}
                     ] -- 1-1
        let expressionTree = Operation2 {
            binaryOperator = Subtraction, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree *" $ do
        let tokens = [
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "*"},
                        LiteralNumber {content = Data.Text.pack "1"}
                     ] -- 1*1
        let expressionTree = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree /" $ do
        let tokens = [
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "/"},
                        LiteralNumber {content = Data.Text.pack "1"}
                     ] -- 1/1
        let expressionTree = Operation2 {
            binaryOperator = Division, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree ++" $ do
        let tokens = [
                        LiteralString {content = Data.Text.pack "hugo"},
                        Operator {name = Data.Text.pack "++"},
                        LiteralString {content = Data.Text.pack "bert"}
                     ] -- "hugo"++"bert"
        let expressionTree = Parskell.ExpressionTree.Operation2 {
            binaryOperator = Concatenation,
            expression1 = Const ConstantString {valueString = Data.Text.pack "hugo"}, 
            expression2 = Const ConstantString {valueString = Data.Text.pack "bert"}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with left2right" $ do
        let tokens = [
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "2"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "3"}
                     ] -- 1+2+3
        let firstCalc = Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 2.0}
        }
        let expressionTree = Operation2 {
            binaryOperator = Addition, 
            expression1 = firstCalc, 
            expression2 = Const ConstantFloat {valueFloat = 3.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with precedence over left2right" $ do
        let tokens = [
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "2"},
                        Operator {name = Data.Text.pack "*"},
                        LiteralNumber {content = Data.Text.pack "3"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "4"}
                     ] -- 1+2*3+4
        let middleCalc = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 2.0}, 
            expression2 = Const ConstantFloat {valueFloat = 3.0}
        }
        let leftCalc = Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = middleCalc
        }
        let expressionTree = Operation2 {
            binaryOperator = Addition, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with brackets over left2right" $ do
        let tokens = [
                        LiteralNumber {content = Data.Text.pack "2"},
                        Operator {name = Data.Text.pack "+"},
                        RoundBracketOpen,
                        LiteralNumber {content = Data.Text.pack "3"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "4"},
                        RoundBracketClose,
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "5"}
                     ] -- 2+(3+4)+5
        let middleCalc = Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 3.0}, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        let leftCalc = Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 2.0}, 
            expression2 = middleCalc
        }
        let expressionTree = Operation2 {
            binaryOperator = Addition, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 5.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with brackets over precedence" $ do
        let tokens = [
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "*"},
                        RoundBracketOpen,
                        LiteralNumber {content = Data.Text.pack "6"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "7"},
                        RoundBracketClose,
                        Operator {name = Data.Text.pack "*"},
                        LiteralNumber {content = Data.Text.pack "4"}
                     ] -- 1*(6+7)*4
        let middleCalc = Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 6.0}, 
            expression2 = Const ConstantFloat {valueFloat = 7.0}
        }
        let leftCalc = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = middleCalc
        }
        let expressionTree = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with unnecessary ignore tokens" $ do
        let tokens = [
                        Ignore,
                        LiteralNumber {content = Data.Text.pack "1"},
                        Ignore,
                        Ignore,
                        Operator {name = Data.Text.pack "*"},
                        Ignore,
                        RoundBracketOpen,
                        Ignore,
                        Ignore,
                        LiteralNumber {content = Data.Text.pack "6"},
                        Ignore,
                        Ignore,
                        Operator {name = Data.Text.pack "+"},
                        Ignore,
                        LiteralNumber {content = Data.Text.pack "7"},
                        RoundBracketClose,
                        Ignore
                     ] --    1  *  (  6  +  7   )   
        let middleCalc = Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 6.0}, 
            expression2 = Const ConstantFloat {valueFloat = 7.0}
        }
        let expressionTree = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = middleCalc
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with unnecessary spaces" $ do
        let tokens = [
                        RoundBracketOpen,
                        RoundBracketOpen,
                        RoundBracketOpen,
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "1"},
                        RoundBracketClose,
                        RoundBracketClose,
                        RoundBracketClose
                     ] -- (((1+1)))
        let expressionTree = Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with fake embracing brackets (with lexer preprocessing)" $ do
        let tokens = [
                        RoundBracketOpen,
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "/"},
                        LiteralNumber {content = Data.Text.pack "2"},
                        RoundBracketClose,
                        Operator {name = Data.Text.pack "/"},
                        RoundBracketOpen,
                        LiteralNumber {content = Data.Text.pack "3"},
                        Operator {name = Data.Text.pack "/"},
                        LiteralNumber {content = Data.Text.pack "4"},
                        RoundBracketClose
                     ] -- (1/2)/(3/4)
        let leftCalc = Operation2 {
            binaryOperator = Division, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 2.0}
        }
        let rightCalc = Operation2 {
            binaryOperator = Division, 
            expression1 = Const ConstantFloat {valueFloat = 3.0}, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        let expressionTree = Operation2 {
            binaryOperator = Division, 
            expression1 = leftCalc, 
            expression2 = rightCalc
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with do-block with single-line expression" $ do
        let tokens = [
                        Do,
                        LiteralString {content = Data.Text.pack "Hugobert"},
                        Newline,
                        LiteralString {content = Data.Text.pack "Sinnloser Text"},
                        Semicolon,
                        Print,
                        LiteralNumber {content = Data.Text.pack "42"},
                        Newline,
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "1"},
                        Done
                     ] -- do things 1+1 done
        let doBlock = DoExpression {
            doStatements = [GenericStatement {statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Hugobert"}]},
                            GenericStatement {statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Sinnloser Text"}]},
                            PrintStatement {printableExpression = Const ConstantFloat {valueFloat = 42.0}}], 
            doExpression = Operation2 {
                                     binaryOperator = Addition, 
                                     expression1 = Const ConstantFloat {valueFloat = 1.0}, 
                                     expression2 = Const ConstantFloat {valueFloat = 1.0}
                                 }
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right doBlock
        
    it "returns a simple expression tree with do-block with multi-line expression" $ do
        let tokens = [
                        Do,
                        LiteralString {content = Data.Text.pack "Hugobert"},
                        Newline,
                        LiteralString {content = Data.Text.pack "Sinnloser Text"},
                        Semicolon,
                        Print,
                        LiteralNumber {content = Data.Text.pack "42"},
                        Newline,
                        LiteralNumber {content = Data.Text.pack "1"},
                        Newline,
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "1"},
                        Done
                     ] -- do things 1+1 done
        let doBlock = DoExpression {
            doStatements = [GenericStatement {statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Hugobert"}]},
                            GenericStatement {statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Sinnloser Text"}]},
                            PrintStatement {printableExpression = Const ConstantFloat {valueFloat = 42.0}}], 
            doExpression = Operation2 {
                                     binaryOperator = Addition, 
                                     expression1 = Const ConstantFloat {valueFloat = 1.0}, 
                                     expression2 = Const ConstantFloat {valueFloat = 1.0}
                                 }
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right doBlock
        
    it "returns a simple expression tree with do-block without statements" $ do
        let tokens = [
                        Do,
                        LiteralNumber {content = Data.Text.pack "1"},
                        Newline,
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "1"},
                        Done
                     ] -- do 1+1 done
        let doBlock = DoExpression {
            doStatements = [], 
            doExpression = Operation2 {
                                     binaryOperator = Addition, 
                                     expression1 = Const ConstantFloat {valueFloat = 1.0}, 
                                     expression2 = Const ConstantFloat {valueFloat = 1.0}
                                 }
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right doBlock
        
    it "returns a simple expression tree with do-block with complex multi-line expression" $ do
        let tokens = [
                        Do,
                        LiteralString {content = Data.Text.pack "Hugobert"},
                        Newline,
                        LiteralString {content = Data.Text.pack "Sinnloser Text"},
                        Semicolon,
                        Print,
                        LiteralNumber {content = Data.Text.pack "42"},
                        Newline,
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "*"},
                        RoundBracketOpen,
                        LiteralNumber {content = Data.Text.pack "6"},
                        Newline,
                        Newline,
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "13"},
                        RoundBracketClose,
                        Newline,
                        Operator {name = Data.Text.pack "*"},
                        LiteralNumber {content = Data.Text.pack "4"},
                        Newline,
                        Done
                     ] -- do things 1*(6+7)*4 done
        let middleCalc = Operation2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 6.0}, 
            expression2 = Const ConstantFloat {valueFloat = 13.0}
        }
        let leftCalc = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = middleCalc
        }
        let expressionTree = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        let doBlock = DoExpression {
            doStatements = [GenericStatement {statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Hugobert"}]},
                            GenericStatement {statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Sinnloser Text"}]},
                            PrintStatement {printableExpression = Const ConstantFloat {valueFloat = 42.0}}], 
            doExpression = expressionTree
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right doBlock
        
    it "returns a simple expression tree with let-expression with single-line expression" $ do
        let tokens = [
                        Let,
                        Identifier {name = Data.Text.pack "a"},
                        Operator {name = Data.Text.pack "="},
                        LiteralNumber {content = Data.Text.pack "42"},
                        In,
                        Identifier {name = Data.Text.pack "a"},
                        Operator {name = Data.Text.pack "*"},
                        LiteralNumber {content = Data.Text.pack "20"},
                        Return
                     ] -- let a = 42 in a ** 20i return
        let expr = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = Assignee {assigneeName = Data.Text.pack "a"}, 
            expression2 = Const ConstantFloat {valueFloat = 20.0}
        }
        let letExpressionBlock = LetExpression {
            letAssignments = [Assignment {
                assignmentIdentifier = Data.Text.pack "a", 
                assignmentExpression = Const ConstantFloat {valueFloat = 42.0}}],
            letExpression = expr
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right letExpressionBlock
        
    it "returns a simple expression tree with let-expression with complex multi-line expression" $ do
        let tokens = [
                        Let,
                        Identifier {name = Data.Text.pack "a"},
                        Operator {name = Data.Text.pack "="},
                        LiteralNumber {content = Data.Text.pack "42"},
                        In,
                        LiteralNumber {content = Data.Text.pack "1"},
                        Operator {name = Data.Text.pack "*"},
                        RoundBracketOpen,
                        Identifier {name = Data.Text.pack "a"},
                        Newline,
                        Newline,
                        Operator {name = Data.Text.pack "+"},
                        LiteralNumber {content = Data.Text.pack "13"},
                        RoundBracketClose,
                        Newline,
                        Operator {name = Data.Text.pack "*"},
                        LiteralNumber {content = Data.Text.pack "4"},
                        Newline,
                        Return
                     ] -- let a = 42 in 1 * (a + 13) * 4) return
        let middleCalc = Operation2 {
            binaryOperator = Addition, 
            expression1 = Assignee {assigneeName = Data.Text.pack "a"}, 
            expression2 = Const ConstantFloat {valueFloat = 13.0}
        }
        let leftCalc = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = middleCalc
        }
        let expr = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        let letExpressionBlock = LetExpression {
            letAssignments = [Assignment {
                assignmentIdentifier = Data.Text.pack "a", 
                assignmentExpression = Const ConstantFloat {valueFloat = 42.0}}],
            letExpression = expr
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right letExpressionBlock
        
    it "returns a simple expression tree with distortion" $ do
        let tokens = [
                        RoundBracketOpen,
                        Newline,
                        RoundBracketOpen,
                        Newline,
                        RoundBracketOpen,
                        Newline,
                        RoundBracketOpen,
                        LiteralNumber {content = Data.Text.pack "1"},
                        RoundBracketClose,
                        Newline,
                        RoundBracketClose,
                        Newline,
                        RoundBracketClose,
                        Newline,
                        RoundBracketClose
                     ] -- (\n(\n(\n(1)\n)\n)\n)
        let expr = Const ConstantFloat {valueFloat = 1.0}
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expr
