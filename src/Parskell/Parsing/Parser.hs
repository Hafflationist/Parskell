module Parskell.Parsing.Parser (parseExpression) where

import Data.Either
import Data.Either.Combinators
import Data.List
import Data.Text
import Text.Read
import Parskell.ExpressionTree
import Parskell.ExpressionTree.Conversion
import Parskell.Lexing.Tokens

import Debug.Trace


readMaybeFloatUnpack :: Text -> Maybe Double
readMaybeFloatUnpack = readMaybe . Data.Text.unpack
readMaybeIntegerUnpack :: Text -> Maybe Integer
readMaybeIntegerUnpack = readMaybe . Data.Text.unpack



hideThingsInParentheses :: [Token] -> [Token]
hideThingsInParentheses =
    let mapper count RoundBracketOpen = (count + 1, Ignore)
        mapper count RoundBracketClose = (count - 1, Ignore)
        mapper count character = (count, if count == 0 then character else Ignore)
    in snd . Data.List.mapAccumL mapper 0



splitOnAndParse :: BinaryOperator -> [Token] -> Either [String] Expression
splitOnAndParse operator tokens = 
    let operatorText = binaryOperator2Text operator
    in do
        indexOfOperatorReverse <- maybeToRight ["Internal parser error! Operator not found!"]
                                . elemIndex (Parskell.Lexing.Tokens.Operator {name = operatorText})
                                . Data.List.reverse
                                . hideThingsInParentheses 
                                $ tokens
        let indexOfOperator = Data.List.length tokens - 1 - indexOfOperatorReverse
        let left = Data.List.take indexOfOperator tokens
        let right = Data.List.drop (indexOfOperator + 1) tokens
--        let right = trace ((show left) ++ " | " ++ (show (Data.List.drop (indexOfOperator + 1) tokens))) (Data.List.drop (indexOfOperator + 1) tokens)
        leftExpression <- parseExpression left
        rightExpression <- parseExpression right
        return (Op2 (Operator2 {binaryOperator = operator, expression1 = leftExpression, expression2 = rightExpression}))



trimParentheses :: [Token] -> [Token]
trimParentheses tokens = 
    let head = Data.List.head tokens
        last = Data.List.last tokens
        shouldTrim = (not . Data.List.any (/= Ignore) . hideThingsInParentheses) tokens
    in if (head == RoundBracketOpen) && (last == RoundBracketClose && shouldTrim)
       then trimParentheses 
          . Data.List.init 
          . Data.List.tail 
          $ tokens
       else tokens



shouldSplitOn :: [Token] -> Token -> Bool
shouldSplitOn tokens operator = operator `elem` hideThingsInParentheses tokens



parseExpression :: [Token] -> Either [String] Expression
parseExpression tokens
    | trimmedTokens `shouldSplitOn` operatorToken "+" = splitOnAndParse Addition trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "-" = splitOnAndParse Subtraction trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "*" = splitOnAndParse Multiplication trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "/" = splitOnAndParse Division trimmedTokens
    where operatorToken op = Parskell.Lexing.Tokens.Operator { name = Data.Text.pack op}
          filterIgnore = Data.List.filter (/= Ignore)
          trimmedTokens = filterIgnore 
                        . trimParentheses 
                        . filterIgnore 
                        $ tokens

parseExpression tokens = parseExpressionConst . trimParentheses $ tokens
    
parseExpressionConst [Parskell.Lexing.Tokens.Literal {content = c}] = do
    value <- maybeToRight ["Could not parse float: " ++ Data.Text.unpack c]
           . readMaybeFloatUnpack 
           $ c
    return (Const (ConstantFloat { valueFloat = value }))
    