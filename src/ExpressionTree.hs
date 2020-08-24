module ExpressionTree where

import Data.Text

newtype Constant = Constant {value :: Double}

data Operator1 = Operator1 {unaryOperator :: Text, expression :: Expression}
data Operator2 = Operator2 {binaryOperator :: Text, expression1 :: Expression, expression2 :: Expression}

data Expression 
    = Const Constant
    | Op1 Operator1
    | Op2 Operator2
    
    

