module Parskell.ExpressionTree.Parser where

import Data.Text
import Text.Read
import Parskell.ExpressionTree

import Debug.Trace



operators :: [Char]
operators = ['+', '-', '*', '/']



readMaybeUnpack :: Text -> Maybe Double
readMaybeUnpack = readMaybe . Data.Text.unpack



splitOnAndParse :: Text -> BinaryOperator -> Text -> Maybe Expression
splitOnAndParse operatorText operator text = 
    let left : rightList = splitOn operatorText text
        right = trace ("both sides: " ++ Data.Text.unpack left ++ " | " ++ (Data.Text.unpack . Data.Text.concat $ rightList)) Data.Text.concat rightList
    in do 
        leftExpression <- parseExpression left
        rightExpression <- parseExpression right
        return (Op2 (Operator2 {binaryOperator = operator, expression1 = leftExpression, expression2 = rightExpression}))



parseExpression :: Text -> Maybe Expression
parseExpression text
    | Data.Text.pack "+" `isInfixOf` text = splitOnAndParse (Data.Text.pack "+") Addition text
    | Data.Text.pack "-" `isInfixOf` text = splitOnAndParse (Data.Text.pack "-") Subtraction text
    | Data.Text.pack "*" `isInfixOf` text = splitOnAndParse (Data.Text.pack "*") Multiplication text
    | Data.Text.pack "/" `isInfixOf` text = splitOnAndParse (Data.Text.pack "/") Division text
           
parseExpression const = do 
    value <- readMaybeUnpack const
    return (Const (Constant { value = value }))
                
