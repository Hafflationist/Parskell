module Parskell.ExpressionTree (
    Expression(..),
    Statement(..),
    BinaryOperator(..),
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
    (==) _ _ = False
instance Show Constant where
    show ConstantFloat {valueFloat = a} = "(" ++ show a ++ ")" 
    show ConstantInteger {valueInteger = a} = "(" ++ show a ++ ")" 
    
    
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


data Statement
    = PrintStatement {printableExpression :: Expression}
    | GenericStatement {statementContent :: Text}
instance Eq Statement where
    (==) PrintStatement {printableExpression = e1} PrintStatement {printableExpression = e2} = e1 == e2
    (==) GenericStatement {statementContent = c1} GenericStatement {statementContent = c2} = c1 == c2
    (==) _ _ = False
instance Show Statement where
    show PrintStatement {printableExpression = e} = "print " ++ show e
    show GenericStatement {statementContent = c} = "stmt " ++ show c

data Expression 
    = Const Constant
    | Operation1 {unaryOperator :: UnaryOperator, expression :: Expression}
    | Operation2 {binaryOperator :: BinaryOperator, expression1 :: Expression, expression2 :: Expression}
    | DoExpression {doStatements :: [Statement], expression :: Expression}
instance Eq Expression where
    (==) (Const a1) (Const a2) = a1 == a2
    (==) Operation1 {unaryOperator = op1, expression = a1} 
         Operation1 {unaryOperator = op2, expression = a2} = 
         op1 == op2 && a1 == a2
    (==) Operation2 {binaryOperator = op1, expression1 = a11, expression2 = a21} 
         Operation2 {binaryOperator = op2, expression1 = a12, expression2 = a22} =
         op1 == op2 && a11 == a12 && a21 == a22
    (==) DoExpression {doStatements = s1, expression = e1} DoExpression {doStatements = s2, expression = e2} =
         s1 == s2 && e1 == e2
    (==) _ _ = False
instance Show Expression where
    show (Const a) = show a
    show Operation1 {unaryOperator = op, expression = a} = "(" ++ show op ++ " " ++ show a ++ ")"
    show Operation2 {binaryOperator = op, expression1 = a1, expression2 = a2} = 
        "(" ++ show a1 ++ " " ++ show op ++ " " ++ show a2 ++ ")"
    show DoExpression {doStatements = ss, expression = expr} =
        " (do [ " ++ show ss ++ " ] " ++ show expr ++ "done) "