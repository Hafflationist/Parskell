module Parskell.Lexing.Lexer (lexing, lexingWord) where

import Data.Either
import Data.List
import Data.Text
import Parskell.Lexing.Tokens


    
operatorChars :: String
operatorChars = ['!', '°', '^', '+', '-', '*', '/', '~', '?','<', '>','=', ':', '.', '|']
numberChars :: String
numberChars = ['0'..'9']
identifierChars :: String
identifierChars = ['A'..'z']



isOperator :: Text -> Bool
isOperator = Data.Text.null 
           . Data.Text.filter (`elem` operatorChars)

isLiteral :: Text -> Bool
isLiteral _ = False

isIdentifier :: Text -> Bool
isIdentifier _ = False
    
    
    
identifier2Keyword :: Token -> Token
identifier2Keyword Identifier {name = n}
    | n == Data.Text.pack "let" = Let
    | n == Data.Text.pack "in" = In 
    | otherwise = Identifier {name = n}
identifier2Keyword i = i



disassemble :: [Char] -> [Token] -> Either String [Token]
disassemble [] acc = Right acc
disassemble (character : word) acc
    | '(' == character = disassemble word (acc ++ [RoundBracketOpen])
    | ')' == character = disassemble word (acc ++ [RoundBracketClose]) 
    | '[' == character = disassemble word (acc ++ [SquareBracketOpen])
    | ']' == character = disassemble word (acc ++ [SquareBracketClose]) 
    | '"' == character = disassembleLiteralString "" word acc
    | character `elem` operatorChars = disassembleOperator [character] word acc
    | character `elem` numberChars = disassembleNumber [character] word acc
    | character `elem` identifierChars = disassembleIdentifier [character] word acc
    | otherwise = return acc
    
disassembleLiteralString :: [Char] -> [Char] -> [Token] -> Either String [Token]
disassembleLiteralString hint [] acc = Left ("'\"" ++ hint ++ "' does not terminate properly!")
disassembleLiteralString hint (character : word) acc =
    if character == '"'
    then disassemble word (acc ++ [Literal {content = Data.Text.pack hint}])
    else disassembleLiteralString (hint ++ [character]) word acc

disassembleOperator :: [Char] -> [Char] -> [Token] -> Either String [Token]
disassembleOperator hint [] acc = Right (acc ++ [Operator {name = Data.Text.pack hint}])
disassembleOperator hint (character : word) acc = 
    if character `elem` operatorChars
    then disassembleOperator (hint ++ [character]) word acc
    else disassemble (character : word) (acc ++ [Operator {name = Data.Text.pack hint}])

disassembleNumber :: [Char] -> [Char] -> [Token] -> Either String [Token]
disassembleNumber hint [] acc = Right (acc ++ [Literal {content = Data.Text.pack hint}])
disassembleNumber hint (character : word) acc
    | character `elem` numberChars || '.' == character = disassembleNumber (hint ++ [character]) word acc
    | character `elem` ['a'..'z'] = disassemble word (acc ++ [Literal {content = Data.Text.pack (hint ++ [character])}])
    | otherwise = disassemble (character : word) (acc ++ [Literal {content = Data.Text.pack hint}])

disassembleIdentifier :: [Char] -> [Char] -> [Token] -> Either String [Token]
disassembleIdentifier hint [] acc = Right (acc ++ [identifier2Keyword Identifier {name = Data.Text.pack hint}])
disassembleIdentifier hint (character : word) acc = 
    if character `elem` identifierChars
    then disassembleIdentifier (hint ++ [character]) word acc
    else disassemble (character : word) (acc ++ [identifier2Keyword Identifier {name = Data.Text.pack hint}])



disassembleInit :: [Char] -> Either String [Token]
disassembleInit word = disassemble word []



lexingWord :: Text -> Either String [Token]
lexingWord word
    | "let" == Data.Text.unpack word = Right [Let]
    | "in" == Data.Text.unpack word = Right [In]
    | otherwise = disassembleInit 
                . Data.Text.unpack 
                $ word



lexing :: Text -> Either [String] [Token]
lexing input = 
    let words = Data.Text.lines input >>= Data.Text.words
        (lefts, rights) = Data.Either.partitionEithers . fmap lexingWord $ words 
    in if Prelude.null lefts 
    then Right (Prelude.concat rights)
    else Left lefts    