{-# LANGUAGE TupleSections #-}

module Parskell.Lexing.Lexer (lexing, lexingWord, lexingWithoutLines) where

import Data.Bifunctor
import Data.Either
import Data.List
import Data.Text
import Parskell.Lexing.Tokens

import Debug.Trace



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
disassembleLiteralString hint [] _ = Left ("'\"" ++ hint ++ "' does not terminate properly!")
disassembleLiteralString hint (character : word) acc =
    if character == '"'
    then disassemble word (acc ++ [LiteralString {content = Data.Text.pack hint}])
    else disassembleLiteralString (hint ++ [character]) word acc

disassembleOperator :: [Char] -> [Char] -> [Token] -> Either String [Token]
disassembleOperator hint [] acc = Right (acc ++ [Operator {name = Data.Text.pack hint}])
disassembleOperator hint (character : word) acc = 
    if character `elem` operatorChars
    then disassembleOperator (hint ++ [character]) word acc
    else disassemble (character : word) (acc ++ [Operator {name = Data.Text.pack hint}])

disassembleNumber :: [Char] -> [Char] -> [Token] -> Either String [Token]
disassembleNumber hint [] acc = Right (acc ++ [LiteralNumber {content = Data.Text.pack hint}])
disassembleNumber hint (character : word) acc
    | character `elem` numberChars || '.' == character = disassembleNumber (hint ++ [character]) word acc
    | character `elem` ['a'..'z'] = disassemble word (acc ++ [LiteralNumber {content = Data.Text.pack (hint ++ [character])}])
    | otherwise = disassemble (character : word) (acc ++ [LiteralNumber {content = Data.Text.pack hint}])

disassembleIdentifier :: [Char] -> [Char] -> [Token] -> Either String [Token]
disassembleIdentifier hint [] acc = Right (acc ++ [identifier2Keyword Identifier {name = Data.Text.pack hint}])
disassembleIdentifier hint (character : word) acc = 
    if character `elem` identifierChars
    then disassembleIdentifier (hint ++ [character]) word acc
    else disassemble (character : word) (acc ++ [identifier2Keyword Identifier {name = Data.Text.pack hint}])



disassembleInit :: [Char] -> Either String [Token]
disassembleInit word = disassemble word []



lexingWordInner :: String -> Token -> Text -> Either String [Token]
lexingWordInner substring subtoken word =
    let subtext = Data.Text.pack substring
        wordTail = Data.Text.intercalate subtext 
                 . Data.List.tail 
                 . Data.Text.splitOn subtext 
                 $ word
    in if Data.Text.null wordTail
    then Right [subtoken]
    else do 
        tokensTail <- lexingWord wordTail
        return (subtoken : tokensTail)



lexingWord :: Text -> Either String [Token]
lexingWord word
    | pa "let" `prefixOf` word = lexingWordInner "let" Let word
    | pa "in" `prefixOf` word = lexingWordInner "in" In word
    | pa "return" `prefixOf` word = lexingWordInner "return" Return word
    | pa "done" `prefixOf` word = lexingWordInner "done" Done word
    | pa "do" `prefixOf` word = lexingWordInner "do" Do word
    | pa "print" `prefixOf` word = lexingWordInner "print" Print word
    | otherwise = disassembleInit 
                . Data.Text.unpack 
                $ word
    where pa = Data.Text.pack
          prefixOf = Data.Text.isPrefixOf



lexingWithoutLines :: Text -> Either [String] [Token]
lexingWithoutLines input = 
    let morpheme = Data.Text.lines input >>= Data.Text.words
        (errors, niceTokens) = Data.Either.partitionEithers . fmap lexingWord $ morpheme 
    in if Prelude.null errors 
    then Right (Prelude.concat niceTokens)
    else Left errors



lexing :: Text -> Either [(Integer, String)] [(Integer, Token)]
lexing input = 
    let counter = [1..]
        inputLines = Data.Text.lines input
        linesWithCount = Data.List.zip counter inputLines
        inputWords = linesWithCount >>= (\ (countNum, line) -> fmap (countNum,) . Data.Text.words $ line)
        (errors, niceTokens) = Data.Either.partitionEithers
                        . fmap ((\ (c, eith) -> Data.Bifunctor.bimap (c,) (c,) eith) 
                             . Data.Bifunctor.second lexingWord) 
                        $ inputWords
    in if Prelude.null errors 
    then Right
       . Data.List.tail
       . Data.List.foldl (\ acc (line, token) -> 
                            let (preLine, _) = Data.List.last acc
                            in acc ++ (if preLine == line 
                                       then [(line, token)] 
                                       else [(preLine, Newline), (line, token)] )) 
                         [(1, Ignore)] 
       $ (niceTokens >>= (\ (c, tokens) -> (c,) <$> tokens))
    else Left errors
