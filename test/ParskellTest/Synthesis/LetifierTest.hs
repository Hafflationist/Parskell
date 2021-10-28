module ParskellTest.Synthesis.LetifierTest where

import Data.Text
import Parskell.ExpressionTree
import Parskell.Lexing.Tokens
import Parskell.Synthesis.Counter as Counter
import Parskell.Synthesis.Letifier
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
        let toTest = Counter.start 0
                   . Parskell.Synthesis.Letifier.letifyNesting
                   $ expressionTree
        toTest @?= expressionTree
        
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
                    assignmentIdentifier = Data.Text.pack "#0", 
                    assignmentExpression = letExpr
                }
            ],
            letExpression = Operation1 {
                unaryOperator = Negation, 
                opExpression = Assignee {assigneeName = Data.Text.pack "#0"}
            }
        }
        let toTest = Counter.start 0
                   . Parskell.Synthesis.Letifier.letifyNesting
                   $ expressionTree
        toTest @?= expressionTreeLetified
        
    it "should letify binary operator" $ do
        let letExpr = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "a", 
                    assignmentExpression = Const ConstantFloat {valueFloat = 42.0}
                }
            ],
            letExpression = Const ConstantFloat {valueFloat = 42.0}
        }
        let expressionTree = Operation2 {
            binaryOperator = Addition, 
            expression1 = letExpr, 
            expression2 = Const ConstantFloat {valueFloat = 42.0}
        }
        let expressionTreeLetified = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "#0", 
                    assignmentExpression = letExpr
                }
            ],
            letExpression = Operation2 {
                binaryOperator = Addition, 
                expression1 = Assignee {assigneeName = Data.Text.pack "#0"}, 
                expression2 = Const ConstantFloat {valueFloat = 42.0}
            }
        }
        let toTest = Counter.start 0
                   . Parskell.Synthesis.Letifier.letifyNesting
                   $ expressionTree
        toTest @?= expressionTreeLetified
        
    it "should letify (2x) binary operator" $ do
        let letExpr1 = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "a", 
                    assignmentExpression = Const ConstantFloat {valueFloat = 42.0}
                }
            ],
            letExpression = Const ConstantFloat {valueFloat = 42.0}
        }
        let letExpr2 = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "b", 
                    assignmentExpression = Const ConstantFloat {valueFloat = 808.0}
                }
            ],
            letExpression = Const ConstantFloat {valueFloat = 808.0}
        }
        let expressionTree = Operation2 {
            binaryOperator = Addition, 
            expression1 = letExpr1, 
            expression2 = letExpr2
        }
        let expressionTreeLetified = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "#0", 
                    assignmentExpression = letExpr1
                },
                Assignment {
                    assignmentIdentifier = Data.Text.pack "#1", 
                    assignmentExpression = letExpr2
                }
            ],
            letExpression = Operation2 {
                binaryOperator = Addition,
                expression1 = Assignee {assigneeName = Data.Text.pack "#0"},
                expression2 = Assignee {assigneeName = Data.Text.pack "#1"}
            }
        }
        let toTest = Counter.start 0
                   . Parskell.Synthesis.Letifier.letifyNesting
                   $ expressionTree
        toTest @?= expressionTreeLetified
        
    it "should letify nested let-expression" $ do
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
        let expressionTreeLetified = LetExpression {
            letAssignments = [
                Assignment {
                    assignmentIdentifier = Data.Text.pack "#0", 
                    assignmentExpression = LetExpression {
                        letAssignments = [
                            Assignment {
                                assignmentIdentifier = Data.Text.pack "#1", 
                                assignmentExpression = middleCalc
                            }
                        ],
                        letExpression = Operation2 {
                            binaryOperator = Multiplication, 
                            expression1 = Const ConstantFloat {valueFloat = 1.0}, 
                            expression2 = Assignee {assigneeName = Data.Text.pack "#1"}
                        }
                    }
                }
            ],
            letExpression = Operation2 {
                binaryOperator = Multiplication,
                expression1 = Assignee {assigneeName = Data.Text.pack "#0"},
                expression2 = Const ConstantFloat {valueFloat = 4.0}
            }
        }
        let toTest = Counter.start 0
                   . Parskell.Synthesis.Letifier.letifyNesting
                   $ expressionTree
        toTest @?= expressionTreeLetified
        
    it "should letify do-expression" $ do
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
        let doExpressionExpressionPart = Operation2 {
            binaryOperator = Multiplication, 
            expression1 = leftCalc, 
            expression2 = Const ConstantFloat {valueFloat = 4.0}
        }
        let expressionTree = DoExpression {
            doStatements = [
                GenericStatement { statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Hugobert"}]},
                GenericStatement { statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Sinnloser Text"}]},
                PrintStatement { printableExpression = Const ConstantFloat {valueFloat = 42.0}}
            ], 
            doExpression = doExpressionExpressionPart
        }
        let expressionTreeLetified = DoExpression {
            doStatements = [
                GenericStatement { statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Hugobert"}]},
                GenericStatement { statementContent = Data.Text.pack . show $ [LiteralString {content = Data.Text.pack "Sinnloser Text"}]},
                PrintStatement { printableExpression = Const ConstantFloat {valueFloat = 42.0}}
            ], 
            doExpression = LetExpression {
                letAssignments = [
                    Assignment {
                        assignmentIdentifier = Data.Text.pack "#0", 
                        assignmentExpression = LetExpression {
                            letAssignments = [
                                Assignment {
                                    assignmentIdentifier = Data.Text.pack "#1", 
                                    assignmentExpression = middleCalc
                                }
                            ],
                            letExpression = Operation2 {
                                binaryOperator = Multiplication, 
                                expression1 = Const ConstantFloat {valueFloat = 1.0}, 
                                expression2 = Assignee {assigneeName = Data.Text.pack "#1"}
                            }
                        }
                    }
                ],
                letExpression = Operation2 {
                    binaryOperator = Multiplication,
                    expression1 = Assignee {assigneeName = Data.Text.pack "#0"},
                    expression2 = Const ConstantFloat {valueFloat = 4.0}
                }
            }
        }
        let toTest = Counter.start 0
                   . Parskell.Synthesis.Letifier.letifyNesting
                   $ expressionTree
        toTest @?= expressionTreeLetified