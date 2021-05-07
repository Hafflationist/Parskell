module Parskell.ExpressionTree (
    Expression(..),
    Assignment(..),
    Statement(..),
    UnaryOperator(..),
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
    | ConstantString {valueString :: Text}
    deriving Eq
instance Show Constant where
    show ConstantFloat {valueFloat = a} = "(" ++ show a ++ ")" 
    show ConstantInteger {valueInteger = a} = "(" ++ show a ++ ")" 
    show ConstantString {valueString = a} = "(\"" ++ show a ++ "\")" 
    
    
data Operator 
    = Unary UnaryOperator
    | Binary BinaryOperator 
    deriving Eq
instance Show Operator where
    show (Unary unop) = show unop
    show (Binary unop) = show unop
    
    
data UnaryOperator 
    = Negation
    | BitNegation
    deriving Eq
instance Show UnaryOperator where
    show Negation = "-"
    show BitNegation = "~"

data BinaryOperator
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Concatenation
    deriving Eq
instance Show BinaryOperator where
    show Addition = "+"
    show Subtraction = "-"
    show Multiplication = "*"
    show Division = "/"
    show Concatenation = "++"


data Statement
    = PrintStatement {printableExpression :: Expression}
    | GenericStatement {statementContent :: Text}
    deriving Eq
instance Show Statement where
    show PrintStatement {printableExpression = e} = "print " ++ show e
    show GenericStatement {statementContent = c} = "stmt " ++ show c
    
    
data Assignment = Assignment {assignmentIdentifier :: Text, assignmentExpression :: Expression}
    deriving Eq
instance Show Assignment where
    show Assignment {assignmentIdentifier = i, assignmentExpression = e} = show i ++ " = " ++ show e

data Expression 
    = Const Constant
    | Assignee {assigneeName :: Text}
    | Operation1 {unaryOperator :: UnaryOperator, opExpression :: Expression}
    | Operation2 {binaryOperator :: BinaryOperator, expression1 :: Expression, expression2 :: Expression}
    | DoExpression {doStatements :: [Statement], doExpression :: Expression}
    | LetExpression {letAssignments :: [Assignment], letExpression :: Expression}
    deriving Eq
instance Show Expression where
    show (Const a) = show a
    show Assignee {assigneeName = n} = show n
    show Operation1 {unaryOperator = op, opExpression = a} = "(" ++ show op ++ " " ++ show a ++ ")"
    show Operation2 {binaryOperator = op, expression1 = a1, expression2 = a2} = 
        "(" ++ show a1 ++ " " ++ show op ++ " " ++ show a2 ++ ")"
    show DoExpression {doStatements = ss, doExpression = expr} =
        " (do [ " ++ show ss ++ " ] " ++ show expr ++ "done) "
    show LetExpression {letAssignments = a1, letExpression = e1} =
        " (let [ " ++ show a1 ++ " ] in " ++ show e1 ++ " return) "