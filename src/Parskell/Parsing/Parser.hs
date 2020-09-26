module Parskell.Parsing.Parser (parseExpression) where

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



hideThingsInParentheses :: Token -> Token -> [Token] -> [Token]
hideThingsInParentheses opening closing tokens =
    let mapper countNum character
          | character == opening = (countNum + 1, Ignore)
          | character == closing = (countNum - 1, Ignore)
          | otherwise = (countNum, if countNum == 0 then character else Ignore) 
    in snd . Data.List.mapAccumL mapper 0 $ tokens
    
    
    
parseExpressionDo :: [Token] -> Either [String] Expression
parseExpressionDo (Parskell.Lexing.Tokens.Do : tokensTail) = 
    let tokensTailClean = hideThingsInParentheses Do Done tokensTail
        (doInternal, tail) = Data.List.break (== Done) tokensTailClean
         -- Zeilen werden durch Semikola oder Zeilenumbrüche getrennt. 
         -- Die letzten n Zeilen, die nicht als Aussagen geparst werden können, werden als Abschlussausdruck betrachtet
    in if Done /= Data.List.head tail
       then Left ["No corresponding 'done' found!"]
       else Left ["Implement me!"]
parseExpressionDo _ = Left ["'do'-block does not begin with token 'do'! This error might be caused by a parser bug"]
    



splitOnAndParse :: BinaryOperator -> [Token] -> Either [String] Expression
splitOnAndParse operator tokens = 
    let operatorText = binaryOperator2Text operator
    in do
        indexOfOperatorReverse <- maybeToRight ["Internal parser error! Operator not found!"]
                                . elemIndex (Parskell.Lexing.Tokens.Operator {name = operatorText})
                                . Data.List.reverse
                                . hideThingsInParentheses RoundBracketOpen RoundBracketClose
                                $ tokens
        let indexOfOperator = Data.List.length tokens - 1 - indexOfOperatorReverse
        let left = Data.List.take indexOfOperator tokens
        let right = Data.List.drop (indexOfOperator + 1) tokens
--        let right = trace ((show left) ++ " | " ++ (show (Data.List.drop (indexOfOperator + 1) tokens))) (Data.List.drop (indexOfOperator + 1) tokens)
        leftExpression <- parseExpression left
        rightExpression <- parseExpression right
        return (Operation2 {binaryOperator = operator, expression1 = leftExpression, expression2 = rightExpression})



trimParentheses :: [Token] -> [Token]
trimParentheses tokens = 
    let firstToken = Data.List.head tokens
        lastToken = Data.List.last tokens
        shouldTrim = (not . Data.List.any (/= Ignore) . hideThingsInParentheses RoundBracketOpen RoundBracketClose) tokens
    in if (firstToken == RoundBracketOpen) && (lastToken == RoundBracketClose && shouldTrim)
       then trimParentheses 
          . Data.List.init 
          . Data.List.tail 
          $ tokens
       else tokens



shouldSplitOn :: [Token] -> Token -> Bool
shouldSplitOn tokens operator = operator `elem` hideThingsInParentheses RoundBracketOpen RoundBracketClose tokens



parseExpression :: [Token] -> Either [String] Expression
parseExpression tokens
    | Do == Data.List.head tokens && Do == Data.List.last tokens = parseExpressionDo tokens
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
    