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
        let tokensRelevant = Data.List.take lastIndex tokensTail -- all tokens without do and done
        let linesRelevant = Data.List.Split.splitWhen (\ e -> e `elem` [Newline, Semicolon]) tokensRelevant
        let lineCount = Data.List.length linesRelevant
        pair <- Data.Either.Combinators.maybeToRight -- TODO Add empty expression!
                  ["No last expression found in do-Block! (Every do-Block needs a last expression as return value)"]
              . Data.List.find 
                  (\ (_, eitherExpression) -> Data.Either.Combinators.isRight eitherExpression)
              . fmap
                  (Data.Bifunctor.second (parseExpression . Data.List.concat)
                     . (`Data.List.splitAt` linesRelevant))
              $ [0..(lineCount - 1)]
        let (statementTokens, eitherExpression) = pair
        lastExpression <- eitherExpression
        let (errors, statements) = Data.Either.partitionEithers . fmap parseStatement $ statementTokens
        if not . Data.List.null $ errors
        then Left . Control.Monad.join $ errors
        else Right (DoExpression {doStatements = statements, doExpression = lastExpression})
parseExpressionDo _ = Left ["'do'-block does not begin with token 'do'! This error might be caused by a parser bug"]
    


parseAssignment :: [Token] -> Either [String] Assignment
parseAssignment tokens =
    let tokensClean = Data.List.filter (\ t -> t `notElem` [Newline, Semicolon, Ignore]) tokens
        getTriplet (a : b : c) = Right (a, b, c)
        getTriplet _ = Left ["Incomplete assignment!"]
        getIdentifierName Identifier { name = nameVal } = Right nameVal
        getIdentifierName  _ = Left ["Identifier expected before '='!"]
    in do
        triplet <- getTriplet tokensClean
        let (tokenIdentifier, tokenOp, tokensExpression) = triplet
        if tokenOp == Operator {name = Data.Text.pack "="}
        then do expression <- parseExpression tokensExpression
                identifierName <- getIdentifierName tokenIdentifier
                return Assignment {assignmentIdentifier = identifierName, assignmentExpression = expression}
        else Left ["Second token schould be '=' in assignment!"]



parseAssignmentsFolder :: Token -> [[Token]] -> [[Token]]
parseAssignmentsFolder tokenCurrent (assTokens : listOfAssignmentTokens)
    | Data.List.head assTokens == Operator {name = Data.Text.pack "="} =
            [Ignore] : (tokenCurrent : assTokens) : listOfAssignmentTokens
    | otherwise = (tokenCurrent : assTokens) : listOfAssignmentTokens
parseAssignmentsFolder _ _ = []
    
parseAssignments :: [Token] -> Either [String] [Assignment]
parseAssignments tokens = 
    let (errors, assignments) = Data.Either.partitionEithers
                              . Data.List.map parseAssignment
                              . Data.List.filter (\ ts -> ts /= [Ignore])
                              . Data.List.foldr parseAssignmentsFolder [[Ignore]] 
                              $ tokens
    in if Prelude.null errors
    then Right assignments
    else Left . Control.Monad.join $ errors



parseExpressionLet :: [Token] -> Either [String] Expression
parseExpressionLet (Let : tokensTail) = 
    let tokensTailClean = hideThingsInParentheses Let Return tokensTail
    in do 
        lastIndex <- Data.Either.Combinators.maybeToRight ["No corresponding 'return' found!"] 
                   . elemIndex Return 
                   $ tokensTailClean
        let tokensRelevant = Data.List.take lastIndex tokensTail -- all tokens without let and return
        inIndex <- Data.Either.Combinators.maybeToRight ["No corresponding 'in' found!"]
                 . elemIndex In
                 . hideThingsInParentheses Let Return 
                 $ tokensRelevant
        let (assignmentTokens, expressionTokensWithIn) = Data.List.splitAt inIndex tokensRelevant
        assignments <- parseAssignments assignmentTokens
        let expressionToken = Data.List.tail expressionTokensWithIn
        if Data.List.null expressionToken
        then Left ["No expression found after 'in'!"]
        else do 
            expression <- parseExpression expressionToken 
            Right LetExpression {letAssignments = assignments, letExpression = expression}
parseExpressionLet _ = Left ["'let'-expression does not begin with token 'let'! This error might be caused by a parser bug"]



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



trimIrrelevantTokens :: [Token] -> [Token]
trimIrrelevantTokens = 
    Data.List.dropWhile (\ t -> t `elem` [Ignore, Newline, Semicolon])
    . Data.List.dropWhileEnd (\ t -> t `elem` [Ignore, Newline, Semicolon])
    
    
    
trimRecursive :: [Token] -> [Token]
trimRecursive tokens =
    let trimRecursiveInner preTokens ts 
            | ts == preTokens = ts 
            | otherwise = trimRecursiveInner ts (trimParentheses . trimIrrelevantTokens $ ts)
    in trimRecursiveInner tokens (trimParentheses . trimIrrelevantTokens $ tokens)



shouldSplitOn :: [Token] -> Token -> Bool
shouldSplitOn tokens operator = operator `elem` hideThingsInParentheses RoundBracketOpen RoundBracketClose tokens



parseExpression :: [Token] -> Either [String] Expression
parseExpression tokens
    | Do == Data.List.head tokens && Done == Data.List.last tokens = parseExpressionDo tokens
    | Let == Data.List.head tokens && Return == Data.List.last tokens = parseExpressionLet tokens
    | trimmedTokens `shouldSplitOn` operatorToken "+" = splitOnAndParse Addition trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "-" = splitOnAndParse Subtraction trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "*" = splitOnAndParse Multiplication trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "/" = splitOnAndParse Division trimmedTokens
    | trimmedTokens `shouldSplitOn` operatorToken "++" = splitOnAndParse Concatenation trimmedTokens
    where operatorToken op = Operator { name = Data.Text.pack op}
          filterIgnore = Data.List.filter (/= Ignore)
          trimmedTokens = filterIgnore 
                        . trimRecursive
                        . filterIgnore 
                        $ tokens

parseExpression tokens = parseExpressionConst . trimRecursive $ tokens
    
parseExpressionConst :: [Token] -> Either [String] Expression
parseExpressionConst [LiteralNumber {content = c}] = do
    value <- maybeToRight ["Could not parse float: " ++ Data.Text.unpack c]
           . readMaybeFloatUnpack 
           $ c
    return (Const (ConstantFloat { valueFloat = value }))
parseExpressionConst [LiteralString {content = c}] = 
    Right (Const (ConstantString { valueString = c }))
parseExpressionConst [Identifier { name = n }] = 
    Right Assignee {assigneeName = n}
parseExpressionConst thing = 
    Left ["Expected constant literal! (" ++ show thing ++ ")"]
