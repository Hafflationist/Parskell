module ParskellTest.ExpressionTree.ParserTest where

import Control.Exception (evaluate)
import Data.Text
import Parskell.ExpressionTree.Parser
import Parskell.ExpressionTree
import Test.Hspec
import Test.HUnit
import Test.QuickCheck


parskellTest = do
    it "returns a simple expression tree +" $ do
        let formula = Data.Text.pack "1+1"
        let expressionTree = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree -" $ do
        let formula = Data.Text.pack "1-1"
        let expressionTree = Op2 Operator2 {
            binaryOperator = Subtraction, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree *" $ do
        let formula = Data.Text.pack "1*1"
        let expressionTree = Op2 Operator2 {
            binaryOperator = Multiplication, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree /" $ do
        let formula = Data.Text.pack "1/1"
        let expressionTree = Op2 Operator2 {
            binaryOperator = Division, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree with left2right" $ do
        let formula = Data.Text.pack "1+2+3"
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
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree with precedence over left2right" $ do
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
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree with brackets over left2right" $ do
        let formula = Data.Text.pack "2+(3+4)+5"
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
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree with brackets over precedence" $ do
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
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree with unnecessary brackets" $ do
        let formula = Data.Text.pack "   1  *  (  6  +  7   )   "
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
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree with unnecessary spaces" $ do
        let formula = Data.Text.pack "(((1+1)))"
        let expressionTree = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
        
    it "returns a simple expression tree with fake embracing brackets" $ do
        let formula = Data.Text.pack "(1/2)/(3/4)"
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
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
