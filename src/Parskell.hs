module Parskell where

import System.IO
import Data.Text
import Text.Read
import Debug.Trace


calcIo = do putStrLn "Formula:"
            hFlush stdout
            input <- getLine
            let maybeResult = calcChar input
            let resultOutputMaybe = maybeResult >>= (\ char -> Just . putStrLn $ ("Result calculated by Parskell: " ++ Data.Text.unpack char))
            defaultReturn resultOutputMaybe               
         where 
             calcChar = calc . Just . Data.Text.pack
             defaultReturn (Just opt) = opt
             defaultReturn Nothing = putStrLn "Input code not be parsed!"


readMaybeUnpack :: Text -> Maybe Double
readMaybeUnpack = readMaybe . Data.Text.unpack



calcAtom :: Char -> Maybe (Double -> Double -> Double)
calcAtom '+' = Just (+)
calcAtom '-' = Just (-)
calcAtom '*' = Just (*)
calcAtom '/' = Just (/)
calcAtom _ = Nothing


 
operators :: [Char]
operators = ['+', '-', '*', '/']



divideFormula :: Text -> Maybe (Text, Char, Text)
divideFormula formula 
    | Data.Text.any (`elem` operators) formula = 
        let right = takeWhileEnd (not . (`elem` operators)) formula
            leftWithOp = dropWhileEnd (not . (`elem` operators)) formula
            left = Data.Text.init leftWithOp
            opChar = Data.Text.last left            
        in Just (left, opChar, right)
    | otherwise = Nothing



calc :: Maybe Text -> Maybe Text
calc (Just formula) 
    | Data.Text.any (`elem` operators) formula = do
        (left, opChar, right) <- divideFormula formula
        calculatedLeft <- calc . Just $ left
        calculatedLeftNumber <- readMaybeUnpack calculatedLeft
        rightNumber <- readMaybeUnpack right
        op <- calcAtom opChar
        return (pack . show . op calculatedLeftNumber $ rightNumber)
    | otherwise = Just formula

calc Nothing = Nothing  
