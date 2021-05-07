module ParskellTest.Synthesis.LetifierTest where

import Data.Text
import Parskell.Synthesis.Letifier
import Parskell.ExpressionTree
import Parskell.Lexing.Tokens
import Test.Hspec
import Test.HUnit

letifierTest = do
    it "should change nothing" $ do
        let expr = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = Assignee {assigneeName = Data.Text.pack "a"}, 
            expression2 = Const ConstantFloat {valueFloat = 20.0}
        }
        let expressionTree = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "a", 
                    assignmentExpression = Const ConstantFloat {valueFloat = 42.0}
                }
            ],
            letExpression = expr
        } -- let a = 42 in a ** 20i return
        Parskell.Synthesis.Letifier.letifyNesting expressionTree @?= expressionTree
        
    it "should letify unary operator" $ do
        let letExpr = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "a", 
                    assignmentExpression = Const ConstantFloat {valueFloat = 42.0}
                }
            ],
            letExpression = Const ConstantFloat {valueFloat = 42.0}
        }
        let expressionTree = Operation1 {
            unaryOperator = Negation, 
            opExpression = letExpr
        }
        let expressionTreeLetified = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "#hugo", 
                    assignmentExpression = LetExpression {
                        letAssignments = [
                            Assignment {
                                assignmentIdentifier = Data.Text.pack "a", 
                                assignmentExpression = Const ConstantFloat {valueFloat = 42.0}
                            }
                        ],
                        letExpression = Const ConstantFloat {valueFloat = 42.0}
                    }
                }
            ],
            letExpression = Operation1 {
                unaryOperator = Negation, 
                opExpression = Assignee {assigneeName = Data.Text.pack "#hugo"}
            }
        }
        Parskell.Synthesis.Letifier.letifyNesting expressionTree @?= expressionTreeLetified