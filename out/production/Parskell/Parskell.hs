module Parskell where

import System.IO
import Data.Text
import Text.Read
import Debug.Trace
import Parskell.ExpressionTree.Evaluation
import Parskell.ExpressionTree.Parser


calcIo = do putStrLn "Formula:"
            hFlush stdout
            input <- getLine
            let maybeResult = calcChar input
            let resultOutputMaybe = maybeResult 
                                    >>= (\ char -> Just . putStrLn $ ("Result calculated by Parskell: " ++ show char))
            defaultReturn resultOutputMaybe               
         where 
             calcChar = parseAndEval . Data.Text.pack
             defaultReturn (Just opt) = opt
             defaultReturn Nothing = putStrLn "Input code not be parsed!"







parseAndEval :: Text -> Maybe Double
parseAndEval formula = 
    Parskell.ExpressionTree.Parser.parseExpression formula 
    >>= Parskell.ExpressionTree.Evaluation.eval








-- Old code:
-- TODO Seperate evaluation from parsing
-- 1. generate expression tree
-- 2. EVALUATION

--calcAtom :: Char -> Maybe (Double -> Double -> Double)
--calcAtom '+' = Just (+)
--calcAtom '-' = Just (-)
--calcAtom '*' = Just (*)
--calcAtom '/' = Just (/)
--calcAtom _ = Nothing
--
-- 
--operators :: [Char]
--operators = ['+', '-', '*', '/']
--
--
--divideFormula :: Text -> Maybe (Text, Char, Text)
--divideFormula formula 
--    | Data.Text.any (`elem` Parskell.operators) formula = 
--        let right = takeWhileEnd (not . (`elem` Parskell.operators)) formula
--            leftWithOp = dropWhileEnd (not . (`elem` Parskell.operators)) formula
--            left = Data.Text.init leftWithOp
--            opChar = Data.Text.last left            
--        in Just (left, opChar, right)
--    | otherwise = Nothing
--
--
--calc :: Maybe Text -> Maybe Text
--calc (Just formula) 
--    | Data.Text.any (`elem` Parskell.operators) formula = do
--        let readMaybeUnpackOld = readMaybe . Data.Text.unpack
--        (left, opChar, right) <- divideFormula formula
--        calculatedLeft <- calc . Just $ left
--        calculatedLeftNumber <- readMaybeUnpackOld calculatedLeft
--        rightNumber <- readMaybeUnpackOld right
--        op <- calcAtom opChar
--        return (pack . show . op calculatedLeftNumber $ rightNumber)
--    | otherwise = Just formula
--
--calc Nothing = Nothing
