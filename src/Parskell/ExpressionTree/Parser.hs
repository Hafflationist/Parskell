module Parskell.ExpressionTree.Parser where

import Data.Text
import Text.Read
import Parskell.ExpressionTree
import Parskell.ExpressionTree.Conversion



readMaybeUnpack :: Text -> Maybe Double
readMaybeUnpack = readMaybe . Data.Text.unpack



hideThingsInParentheses :: Text -> Text
hideThingsInParentheses =
    let mapper count '(' = (count + 1, ' ')
        mapper count ')' = (count - 1, ' ')
        mapper count character = (count, if count == 0 then character else ' ')
    in snd . Data.Text.mapAccumL mapper 0



splitOnAndParse :: BinaryOperator -> Text -> Maybe Expression
splitOnAndParse operator text = 
    let operatorText = binaryOperator2Text operator
        length = Data.Text.length
               . Data.Text.intercalate operatorText
               . Prelude.init
               . splitOn operatorText 
               . hideThingsInParentheses 
               $ text
        left = Data.Text.take length text
        right = Data.Text.drop (length + Data.Text.length operatorText) text
    in do 
        leftExpression <- parseExpression left
        rightExpression <- parseExpression right
        return (Op2 (Operator2 {binaryOperator = operator, expression1 = leftExpression, expression2 = rightExpression}))



trimParentheses :: Text -> Text
trimParentheses text = 
    let head = Data.Text.head text
        last = Data.Text.last text
    in if (head == '(') && (last == ')')
       then Data.Text.init . Data.Text.tail $ text
       else text



shouldSplitOn :: Text -> Text -> Bool
shouldSplitOn formula operator = operator `isInfixOf` hideThingsInParentheses formula



parseExpression :: Text -> Maybe Expression
parseExpression formula
    | trimmedFormula `shouldSplitOn` Data.Text.pack "+" = splitOnAndParse Addition trimmedFormula
    | trimmedFormula `shouldSplitOn` Data.Text.pack "-" = splitOnAndParse Subtraction trimmedFormula
    | trimmedFormula `shouldSplitOn` Data.Text.pack "*" = splitOnAndParse Multiplication trimmedFormula
    | trimmedFormula `shouldSplitOn` Data.Text.pack "/" = splitOnAndParse Division trimmedFormula
    where trimmedFormula = trimParentheses formula
           
parseExpression const = do 
    value <- readMaybeUnpack . trimParentheses $ const
    return (Const (Constant { value = value }))
