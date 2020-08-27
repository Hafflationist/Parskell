module Parskell.ExpressionTree (Expression(..), BinaryOperator(..), Operation2(..), Operator(..), Constant(..)) where

import Data.Text

data Constant = Constant {value :: Double}

data Operation1 = Operator1 {unaryOperator :: UnaryOperator, expression :: Expression}
data Operation2 = Operator2 {binaryOperator :: BinaryOperator, expression1 :: Expression, expression2 :: Expression}

data Operator 
    = Unary UnaryOperator
    | Binary BinaryOperator 

data UnaryOperator 
    = Negation
    | BitNegation

data BinaryOperator
    = Addition
    | Subtraction
    | Multiplication
    | Division

data Expression 
    = Const Constant
    | Op1 Operation1
    | Op2 Operation2
    