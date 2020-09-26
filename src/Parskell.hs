module Parskell where

import Data.Either
import Data.Either.Combinators
import Data.Maybe
import Data.Text
import System.IO
import Parskell.Synthesis.Evaluation
import Parskell.Parsing.Parser
import Parskell.Lexing.Lexer


calcIo = do putStrLn "Formula:"
            hFlush stdout
            input <- getLine
            let maybeResult = calcChar input
            let e = maybeResult >>= (\ char -> Right . putStrLn $ ("Result calculated by Parskell: " ++ show char))
            either (\ _ -> putStrLn "Input code not be parsed!") id e
         where 
             calcChar = parseAndEval . Data.Text.pack
             
             
             
parseAndEval :: Text -> Either [String] Double
parseAndEval formula = 
    Parskell.Lexing.Lexer.lexingWithoutLines formula
    >>= Parskell.Parsing.Parser.parseExpression 
    >>= Parskell.Synthesis.Evaluation.eval
