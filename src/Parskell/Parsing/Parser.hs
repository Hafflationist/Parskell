module Parskell.Parsing.Parser (parseExpression) where

import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.Either.Combinators
import Data.List
import Data.List.Split
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
          | character == opening && countNum >= 0 = (countNum + 1, Ignore)
          | character == closing && countNum > 0 = (countNum - 1, Ignore)
          | character == closing && countNum == 0 = (countNum - 1, closing)
          | countNum < 0 = (-1, Ignore)
          | otherwise = (countNum, if countNum == 0 then character else Ignore) 
    in snd . Data.List.mapAccumL mapper 0 $ tokens



parseStatement :: [Token] -> Either [String] Statement
parseStatement (Print : tokensTail) = do 
    toBePrinted <- parseExpression tokensTail
    Right PrintStatement {printableExpression = toBePrinted}
parseStatement tokens = Right GenericStatement {statementContent = Data.Text.pack . show $ tokens}

    
    
parseExpressionDo :: [Token] -> Either [String] Expression
parseExpressionDo (Parskell.Lexing.Tokens.Do : tokensTail) = 
    let tokensTailClean = hideThingsInParentheses Do Done tokensTail
    in do
        lastIndex <- Data.Either.Combinators.maybeToRight ["No corresponding 'done' found!"] 
                   . elemIndex Done 
                   $ tokensTailClean
        let relevantTokens = Data.List.take lastIndex tokensTail -- all tokens without do and done
        let relevantLines = Data.List.Split.splitWhen (\ e -> e `elem` [Newline, Semicolon]) relevantTokens
        let lineCount = Data.List.length relevantLines
        pair <- Data.Either.Combinators.maybeToRight 
                  ["No last expression found in do-Block! (Every do-Block need an last expression as return value)"]
              . Data.List.find 
                  (\ (_, eitherExpression) -> Data.Either.Combinators.isRight eitherExpression)
              . fmap
                  (Data.Bifunctor.second (parseExpression . Data.List.concat)
                     . (`Data.List.splitAt` relevantLines))
              $ [0..(lineCount - 1)]
        let (statementTokens, eitherExpression) = trace (show pair) pair
        lastExpression <- eitherExpression
        let (errors, statements) = Data.Either.partitionEithers . fmap parseStatement $ statementTokens
        if not . Data.List.null $ errors
        then Left . Control.Monad.join $ errors
        else Right (DoExpression {doStatements = statements, expression = lastExpression})
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
    | Do == Data.List.head tokens && Done == Data.List.last tokens = parseExpressionDo tokens
    | trimmedTokens `shouldSplitOn` operatorToken "+" = splitOnAndParse Addition trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "-" = splitOnAndParse Subtraction trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "*" = splitOnAndParse Multiplication trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "/" = splitOnAndParse Division trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "++" = splitOnAndParse Concatenation trimmedTokens
    where operatorToken op = Parskell.Lexing.Tokens.Operator { name = Data.Text.pack op}
          filterIgnore = Data.List.filter (/= Ignore)
          trimmedTokens = filterIgnore 
                        . trimParentheses 
                        . filterIgnore 
                        $ tokens

parseExpression tokens = parseExpressionConst . trimParentheses $ tokens
    
parseExpressionConst :: [Token] -> Either [String] Expression
parseExpressionConst [Parskell.Lexing.Tokens.LiteralNumber {content = c}] = do
    value <- maybeToRight ["Could not parse float: " ++ Data.Text.unpack c]
           . readMaybeFloatUnpack 
           $ c
    return (Const (ConstantFloat { valueFloat = value }))
parseExpressionConst [Parskell.Lexing.Tokens.LiteralString {content = c}] = 
    Right (Const (ConstantString { valueString = c }))
parseExpressionConst _ = 
    Left ["Expected constant literal!"]
