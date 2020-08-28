module Parskell.ExpressionTree.Parser where

import Data.Text
import Text.Read
import Parskell.ExpressionTree
import Parskell.ExpressionTree.Conversion



readMaybeUnpack :: Text -> Maybe Double
readMaybeUnpack = readMaybe . Data.Text.unpack



splitOnAndParse :: Text -> BinaryOperator -> Text -> Maybe Expression
splitOnAndParse operatorText operator text = 
    let left : rightList = splitOn operatorText text
        right = Data.Text.intercalate (binaryOperator2Text operator) rightList
    in do 
        leftExpression <- parseExpression left
        rightExpression <- parseExpression right
        return (Op2 (Operator2 {binaryOperator = operator, expression1 = leftExpression, expression2 = rightExpression}))



shouldSplitOn :: Text -> Text -> Bool
shouldSplitOn formula operator = operator `isInfixOf` formula


parseExpression :: Text -> Maybe Expression
parseExpression text
    | text `shouldSplitOn` Data.Text.pack "+"  = splitOnAndParse (Data.Text.pack "+") Addition text
    | text `shouldSplitOn` Data.Text.pack "-"  = splitOnAndParse (Data.Text.pack "-") Subtraction text
    | text `shouldSplitOn` Data.Text.pack "*"  = splitOnAndParse (Data.Text.pack "*") Multiplication text
    | text `shouldSplitOn` Data.Text.pack "/"  = splitOnAndParse (Data.Text.pack "/") Division text
           
parseExpression const = do 
    value <- readMaybeUnpack const
    return (Const (Constant { value = value }))
                
