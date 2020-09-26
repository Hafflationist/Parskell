module Parskell.Synthesis.Evaluation where

import Parskell.ExpressionTree



applyOperator :: Expression -> Expression -> (Double -> Double -> Double) -> Either [String] Double
applyOperator exp1 exp2 op = do 
    a <- eval exp1
    b <- eval exp2
    return (op a b)



eval :: Expression -> Either [String] Double
eval (Const ConstantFloat { valueFloat = v }) = Right v

eval (Op2 Operator2 { binaryOperator = Addition, expression1 = exp1, expression2 = exp2 }) = applyOperator exp1 exp2 (+)

eval (Op2 Operator2 { binaryOperator = Subtraction, expression1 = exp1, expression2 = exp2 }) = applyOperator exp1 exp2 (-)

eval (Op2 Operator2 { binaryOperator = Multiplication, expression1 = exp1, expression2 = exp2 }) = applyOperator exp1 exp2 (*)

eval (Op2 Operator2 { binaryOperator = Division, expression1 = exp1, expression2 = exp2 }) = applyOperator exp1 exp2 (/)

eval x = Left ["Could not evaluate '" ++ show x ++ "'"]  -- TODO implement type checking and evaluation for integer literals
