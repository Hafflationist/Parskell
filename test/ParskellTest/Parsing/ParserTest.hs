module ParskellTest.Parsing.ParserTest where

import Control.Exception (evaluate)
import Data.Either
import Data.Text
import Parskell.Parsing.Parser
import Parskell.ExpressionTree
import Parskell.Lexing.Lexer
import Parskell.Lexing.Tokens
import Test.Hspec
import Test.HUnit
import Test.QuickCheck


parskellTest = do
    it "returns a simple expression tree +" $ do
        let tokens = [
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "1"}
                     ] -- 1+1
        let expressionTree = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree -" $ do
        let tokens = [
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "-"},
                        Literal {content = Data.Text.pack "1"}
                     ] -- 1-1
        let expressionTree = Op2 Operator2 {
            binaryOperator = Subtraction, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree *" $ do
        let tokens = [
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "*"},
                        Literal {content = Data.Text.pack "1"}
                     ] -- 1*1
        let expressionTree = Op2 Operator2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree /" $ do
        let tokens = [
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "/"},
                        Literal {content = Data.Text.pack "1"}
                     ] -- 1/1
        let expressionTree = Op2 Operator2 {
            binaryOperator = Division, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with left2right" $ do
        let tokens = [
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "2"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "3"}
                     ] -- 1+2+3
        let firstCalc = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 2.0}
        }
        let expressionTree = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = firstCalc, 
            expression2 = Const ConstantFloat {valueFloat = 3.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with precedence over left2right" $ do
        let tokens = [
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "2"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "*"},
                        Literal {content = Data.Text.pack "3"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "4"}
                     ] -- 1+2*3+4
        let formula = Data.Text.pack "1+2*3+4"
        let middleCalc = Op2 Operator2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 2.0}, 
            expression2 = Const ConstantFloat {valueFloat = 3.0}
        }
        let leftCalc = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = middleCalc
        }
        let expressionTree = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with brackets over left2right" $ do
        let tokens = [
                        Literal {content = Data.Text.pack "2"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        RoundBracketOpen,
                        Literal {content = Data.Text.pack "3"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "4"},
                        RoundBracketClose,
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "5"}
                     ] -- 2+(3+4)+5
        let middleCalc = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 3.0}, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        let leftCalc = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 2.0}, 
            expression2 = middleCalc
        }
        let expressionTree = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 5.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with brackets over precedence" $ do
        let tokens = [
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "*"},
                        RoundBracketOpen,
                        Literal {content = Data.Text.pack "6"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "7"},
                        RoundBracketClose,
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "*"},
                        Literal {content = Data.Text.pack "4"}
                     ] -- 1*(6+7)*4
        let formula = Data.Text.pack "1*(6+7)*4"
        let middleCalc = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 6.0}, 
            expression2 = Const ConstantFloat {valueFloat = 7.0}
        }
        let leftCalc = Op2 Operator2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = middleCalc
        }
        let expressionTree = Op2 Operator2 {
            binaryOperator = Multiplication, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with unnecessary ignore tokens" $ do
        let tokens = [
                        Ignore,
                        Literal {content = Data.Text.pack "1"},
                        Ignore,
                        Ignore,
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "*"},
                        Ignore,
                        RoundBracketOpen,
                        Ignore,
                        Ignore,
                        Literal {content = Data.Text.pack "6"},
                        Ignore,
                        Ignore,
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Ignore,
                        Literal {content = Data.Text.pack "7"},
                        RoundBracketClose,
                        Ignore
                     ] --    1  *  (  6  +  7   )   
        let middleCalc = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 6.0}, 
            expression2 = Const ConstantFloat {valueFloat = 7.0}
        }
        let expressionTree = Op2 Operator2 {
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
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "+"},
                        Literal {content = Data.Text.pack "1"},
                        RoundBracketClose,
                        RoundBracketClose,
                        RoundBracketClose
                     ] -- (((1+1)))
        let expressionTree = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
        
    it "returns a simple expression tree with fake embracing brackets (with lexer preprocessing)" $ do
        let tokens = [
                        RoundBracketOpen,
                        Literal {content = Data.Text.pack "1"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "/"},
                        Literal {content = Data.Text.pack "2"},
                        RoundBracketClose,
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "/"},
                        RoundBracketOpen,
                        Literal {content = Data.Text.pack "3"},
                        Parskell.Lexing.Tokens.Operator {name = Data.Text.pack "/"},
                        Literal {content = Data.Text.pack "4"},
                        RoundBracketClose
                     ] -- (1/2)/(3/4)
        let leftCalc = Op2 Operator2 {
            binaryOperator = Division, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 2.0}
        }
        let rightCalc = Op2 Operator2 {
            binaryOperator = Division, 
            expression1 = Const ConstantFloat {valueFloat = 3.0}, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        let expressionTree = Op2 Operator2 {
            binaryOperator = Division, 
            expression1 = leftCalc, 
            expression2 = rightCalc
        }
        Parskell.Parsing.Parser.parseExpression tokens @?= Right expressionTree
