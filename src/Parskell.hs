module Parskell where

import System.IO
import Data.Text
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
