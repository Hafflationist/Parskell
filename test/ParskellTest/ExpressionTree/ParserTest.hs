module ParskellTest.ExpressionTree.ParserTest where

import Control.Exception (evaluate)
import Data.Text
import Parskell.ExpressionTree.Parser
import Parskell.ExpressionTree
import Test.Hspec
import Test.HUnit
import Test.QuickCheck


parskellTest = do
    it "returns a simple expression tree" $ do
        let formula = Data.Text.pack "1+1"
        let expressionTree = Op2 Operator2 {
            binaryOperator = Addition, 
            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
            expression2 = Const ConstantFloat {valueFloat = 1.0}
        }  
        Parskell.ExpressionTree.Parser.parseExpression formula @?= Just expressionTree
