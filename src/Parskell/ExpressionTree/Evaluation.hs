module Parskell.ExpressionTree.Evaluation where

import Parskell.ExpressionTree


applyOperator :: Expression -> Expression -> (Double -> Double -> Double) -> Maybe Double
applyOperator exp1 exp2 op = do 
    a <- eval exp1
    b <- eval exp2
    return (op a b)



eval :: Expression -> Maybe Double
eval (Const Constant { value = v }) = Just v
eval (Op2 Operator2 { binaryOperator = Addition, expression1 = exp1, expression2 = exp2 }) = applyOperator exp1 exp2 (+)

eval (Op2 Operator2 { binaryOperator = Subtraction, expression1 = exp1, expression2 = exp2 }) = applyOperator exp1 exp2 (-)

eval (Op2 Operator2 { binaryOperator = Multiplication, expression1 = exp1, expression2 = exp2 }) = applyOperator exp1 exp2 (*)

eval (Op2 Operator2 { binaryOperator = Division, expression1 = exp1, expression2 = exp2 }) = applyOperator exp1 exp2 (/)

eval _ = Nothing
