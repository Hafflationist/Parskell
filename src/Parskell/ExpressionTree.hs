module Parskell.ExpressionTree (
    Expression(..),
    Assignment(..),
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
    | ConstantString {valueString :: Text}
instance Eq Constant where
    (==) ConstantFloat {valueFloat = a} ConstantFloat {valueFloat = b} = a == b
    (==) ConstantInteger {valueInteger = a} ConstantInteger {valueInteger = b} = a == b 
    (==) ConstantString {valueString = a} ConstantString {valueString = b} = a == b 
    (==) _ _ = False
instance Show Constant where
    show ConstantFloat {valueFloat = a} = "(" ++ show a ++ ")" 
    show ConstantInteger {valueInteger = a} = "(" ++ show a ++ ")" 
    show ConstantString {valueString = a} = "(\"" ++ show a ++ "\")" 
    
    
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
    | Concatenation
instance Eq BinaryOperator where
    (==) Addition Addition = True
    (==) Subtraction Subtraction = True
    (==) Multiplication Multiplication = True
    (==) Division Division = True
    (==) Concatenation Concatenation = True
    (==) _ _ = False
instance Show BinaryOperator where
    show Addition = "+"
    show Subtraction = "-"
    show Multiplication = "*"
    show Division = "/"
    show Concatenation = "++"


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
    
    
data Assignment = Assignment {assignmentIdentifier :: Text, assignmentExpression :: Expression}
instance Eq Assignment where
    (==) Assignment {assignmentIdentifier = i1, assignmentExpression = e1} 
         Assignment {assignmentIdentifier = i2, assignmentExpression = e2} = i1 == i2 && e1 == e2
instance Show Assignment where
    show Assignment {assignmentIdentifier = i, assignmentExpression = e} = show i ++ " = " ++ show e

data Expression 
    = Const Constant
    | Assignee {assigneeName :: Text}
    | Operation1 {unaryOperator :: UnaryOperator, opExpression :: Expression}
    | Operation2 {binaryOperator :: BinaryOperator, expression1 :: Expression, expression2 :: Expression}
    | DoExpression {doStatements :: [Statement], doExpression :: Expression}
    | LetExpression {letAssignments :: [Assignment], letExpression :: Expression}
instance Eq Expression where
    (==) (Const a1) (Const a2) = a1 == a2
    (==) Assignee {assigneeName = n1} Assignee {assigneeName = n2} = n1 == n2
    (==) Operation1 {unaryOperator = op1, opExpression = a1} 
         Operation1 {unaryOperator = op2, opExpression = a2} = 
         op1 == op2 && a1 == a2
    (==) Operation2 {binaryOperator = op1, expression1 = a11, expression2 = a21} 
         Operation2 {binaryOperator = op2, expression1 = a12, expression2 = a22} =
         op1 == op2 && a11 == a12 && a21 == a22
    (==) DoExpression {doStatements = s1, doExpression = e1} 
         DoExpression {doStatements = s2, doExpression = e2} =
         s1 == s2 && e1 == e2
    (==) LetExpression {letAssignments = a1, letExpression = e1} 
         LetExpression {letAssignments = a2, letExpression = e2} =
         a1 == a2 && e1 == e2
    (==) _ _ = False
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