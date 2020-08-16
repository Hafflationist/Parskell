module Lib where

import System.IO
import Data.Text
import Text.Read
import Debug.Trace


someFunc = do putStrLn "Gib Eingabe diggi:"
              hFlush stdout
              input <- getLine
              let maybeResult = calcChar input
              let resultOutputMaybe = maybeResult >>= (\ char -> Just . putStrLn $ ("D1 Eingabe war:" ++ Data.Text.unpack char))
              defaultReturn resultOutputMaybe               
           where 
              calcChar = calc . Just . Data.Text.pack
              defaultReturn (Just opt) = opt
              defaultReturn Nothing = putStrLn "Eingabe konnte nicht interpretiert werden!"

calc :: Maybe Text -> Maybe Text
calc (Just formula) 
    | Data.Text.any (`elem` operators) formula = do
        let right = takeWhileEnd (not . (`elem` operators)) formula
        let leftPart = dropWhileEnd (not . (`elem` operators)) formula
        calculatedLeft <- calc . Just . Data.Text.init $ leftPart
        let readMaybeUnpack = readMaybe . Data.Text.unpack :: Text -> Maybe Double
        calculatedLeftNumber <- readMaybeUnpack calculatedLeft
        rightNumber <- readMaybeUnpack right
        op <- calcAtom . Data.Text.last $ leftPart
        return (pack . show . op calculatedLeftNumber $ rightNumber)
    | otherwise = Just formula      

calc Nothing = Nothing  
    
calcAtom :: Char -> Maybe (Double -> Double -> Double)
calcAtom '+' = Just (+)
calcAtom '-' = Just (-)
calcAtom '*' = Just (*)
calcAtom '/' = Just (/)
calcAtom _ = Nothing
 
operators :: [Char]
operators = ['+', '-', '*', '/']
