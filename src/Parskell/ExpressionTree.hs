module Parskell.ExpressionTree (
    Expression(..), 
    BinaryOperator(..), 
    Operation2(..), 
    Operator(..), 
    Constant(..), 
    TType(..)) where

import Data.Text


data TType 
    = TInteger
    | TFloat
    | TRecord {member :: [(TType, Text)]}


data Constant
    = ConstantFloat {valueFloat :: Double}
    | ConstantInteger {valueInteger :: Integer}
instance Eq Constant where
    (==) ConstantFloat {valueFloat = a} ConstantFloat {valueFloat = b} = a == b
    (==) ConstantInteger {valueInteger = a} ConstantInteger {valueInteger = b} = a == b 
instance Show Constant where
    show ConstantFloat {valueFloat = a} = "(" ++ show a ++ ")" 
    show ConstantInteger {valueInteger = a} = "(" ++ show a ++ ")" 

data Operation1 = Operator1 {unaryOperator :: UnaryOperator, expression :: Expression}
instance Eq Operation1 where
    (==) Operator1 {unaryOperator = op1, expression = a1} 
         Operator1 {unaryOperator = op2, expression = a2} = 
         op1 == op2 && a1 == a2
instance Show Operation1 where
    show Operator1 {unaryOperator = op, expression = a} = "(" ++ show op ++ " " ++ show a ++ ")"
    
    
data Operation2 = Operator2 {binaryOperator :: BinaryOperator, expression1 :: Expression, expression2 :: Expression}
instance Eq Operation2 where
    (==) Operator2 {binaryOperator = op1, expression1 = a11, expression2 = a21} 
         Operator2 {binaryOperator = op2, expression1 = a12, expression2 = a22} =
         op1 == op2 && a11 == a12 && a21 == a22
instance Show Operation2 where
    show Operator2 {binaryOperator = op, expression1 = a1, expression2 = a2} = 
        "(" ++ show a1 ++ " " ++ show op ++ " " ++ show a2 ++ ")"
    
    
data Operator 
    = Unary UnaryOperator
    | Binary BinaryOperator 
instance Eq Operator where
    (==) (Unary a1) (Unary a2) = a1 == a2
    (==) _ _ = False
instance Show Operator where
    show (Unary unop) = show unop
    show (Binary unop) = show unop
    
    
data UnaryOperator 
    = Negation
    | BitNegation
instance Eq UnaryOperator where
    (==) Negation Negation = True
    (==) BitNegation BitNegation = True
    (==) _ _ = False
instance Show UnaryOperator where
    show Negation = "-"
    show BitNegation = "~"

data BinaryOperator
    = Addition
    | Subtraction
    | Multiplication
    | Division
instance Eq BinaryOperator where
    (==) Addition Addition = True
    (==) Subtraction Subtraction = True
    (==) Multiplication Multiplication = True
    (==) Division Division = True
    (==) _ _ = False
instance Show BinaryOperator where
    show Addition = "+"
    show Subtraction = "-"
    show Multiplication = "*"
    show Division = "/"


data Expression 
    = Const Constant
    | Op1 Operation1
    | Op2 Operation2
instance Eq Expression where
    (==) (Const a1) (Const a2) = a1 == a2
    (==) (Op1 a1) (Op1 a2) = a1 == a2
    (==) (Op2 a1) (Op2 a2) = a1 == a2
    (==) _ _ = False
instance Show Expression where
    show (Const a) = show a
    show (Op1 a) = show a
    show (Op2 a) = show a