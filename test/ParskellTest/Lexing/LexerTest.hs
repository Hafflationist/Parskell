module ParskellTest.Lexing.LexerTest where

import Control.Exception (evaluate)
import Data.Text
import Data.Either
import Parskell.Lexing.Tokens
import Parskell.Lexing.Lexer
import Test.Hspec
import Test.HUnit
import Test.QuickCheck


lexerTest = do
    describe "Single words" $ do
        describe "Single" $ do
            it "returns a simple token (Literal number)" $ do
                let word = Data.Text.pack "124"
                let token = Literal {content = Data.Text.pack "124"}
                Parskell.Lexing.Lexer.lexingWord word @?= Right [token]
                
            it "returns a simple token (Literal fractional)" $ do
                let word = Data.Text.pack "124.9"
                let token = Literal {content = Data.Text.pack "124.9"}
                Parskell.Lexing.Lexer.lexingWord word @?= Right [token]
                
            it "returns a simple token (Literal fractional with suffix)" $ do
                let word = Data.Text.pack "124.9f"
                let token = Literal {content = Data.Text.pack "124.9f"}
                Parskell.Lexing.Lexer.lexingWord word @?= Right [token]
                
            it "returns a simple token (Literal string)" $ do
                let word = Data.Text.pack "\"hugo\""
                let token = Literal {content = Data.Text.pack "hugo"}
                Parskell.Lexing.Lexer.lexingWord word @?= Right [token]
                
            it "returns a simple token (Literal string fail)" $ do
                let word = Data.Text.pack "\"hugo"
                (Data.Either.isLeft . Parskell.Lexing.Lexer.lexingWord $ word) @?= True
                
            it "returns a simple token (Operator)" $ do
                let word = Data.Text.pack "|>"
                let token = Operator {name = Data.Text.pack "|>"}
                Parskell.Lexing.Lexer.lexingWord word @?= Right [token]
                
            it "returns a simple token (Identifier)" $ do
                let word = Data.Text.pack "hugobert"
                let token = Identifier {name = Data.Text.pack "hugobert"}
                Parskell.Lexing.Lexer.lexingWord word @?= Right [token]
                
        describe "Compounds" $ do
            it "returns a simple token list (1+1)" $ do
                let word = Data.Text.pack "1+1"
                let tokens = [
                             Literal {content = Data.Text.pack "1"}, 
                             Operator {name = Data.Text.pack "+"}, 
                             Literal {content = Data.Text.pack "1"}
                             ]
                Parskell.Lexing.Lexer.lexingWord word @?= Right tokens
                
            it "returns a simple token list ((alpha>>=beta))" $ do
                let word = Data.Text.pack "(alpha>>=beta)"
                let tokens = [
                             RoundBracketOpen,
                             Identifier {name = Data.Text.pack "alpha"}, 
                             Operator {name = Data.Text.pack ">>="}, 
                             Identifier {name = Data.Text.pack "beta"},
                             RoundBracketClose
                             ]
                Parskell.Lexing.Lexer.lexingWord word @?= Right tokens

    describe "Multiple words" $ do
        it "returns a simple token ((alpha >>= beta))" $ do
                let line = Data.Text.pack "(alpha >>= beta)"
                let tokens = [
                             RoundBracketOpen,
                             Identifier {name = Data.Text.pack "alpha"}, 
                             Operator {name = Data.Text.pack ">>="}, 
                             Identifier {name = Data.Text.pack "beta"},
                             RoundBracketClose
                             ]
                Parskell.Lexing.Lexer.lexingWithoutLines line @?= Right tokens
                
        it "returns a simple token (let a <- 0 in a ** 20i)" $ do
                let line = Data.Text.pack "let a <- 0 in a ** 20i"
                let tokens = [
                             Let,
                             Identifier {name = Data.Text.pack "a"}, 
                             Operator {name = Data.Text.pack "<-"}, 
                             Literal {content = Data.Text.pack "0"},
                             In,
                             Identifier {name = Data.Text.pack "a"}, 
                             Operator {name = Data.Text.pack "**"}, 
                             Literal {content = Data.Text.pack "20i"}
                             ]
                Parskell.Lexing.Lexer.lexingWithoutLines line @?= Right tokens
                
        it "returns a simple token ([let a <- 0 in a ** 20i])" $ do
                let line = Data.Text.pack "[let a <- 0 in a ** 20i]"
                let tokens = [
                             SquareBracketOpen,
                             Let,
                             Identifier {name = Data.Text.pack "a"}, 
                             Operator {name = Data.Text.pack "<-"}, 
                             Literal {content = Data.Text.pack "0"},
                             In,
                             Identifier {name = Data.Text.pack "a"}, 
                             Operator {name = Data.Text.pack "**"}, 
                             Literal {content = Data.Text.pack "20i"},
                             SquareBracketClose
                             ]
                Parskell.Lexing.Lexer.lexingWithoutLines line @?= Right tokens
                
    describe "Multiple words with line count" $ do
        it "returns a simple token ((alpha >>= beta))" $ do
                let line = Data.Text.pack "(alpha \n\
                                          \ >>= beta)"
                let tokens = [
                             (1, RoundBracketOpen),
                             (1, Identifier {name = Data.Text.pack "alpha"}), 
                             (2, Operator {name = Data.Text.pack ">>="}),
                             (2, Identifier {name = Data.Text.pack "beta"}),
                             (2, RoundBracketClose)
                             ]
                Parskell.Lexing.Lexer.lexing line @?= Right tokens
                
        it "returns a simple token (let a <- 0 in a ** 20i)" $ do
                let line = Data.Text.pack "let \n\
                                          \a <- 0 \n\
                                          \in a ** 20i"
                let tokens = [
                             (1, Let),
                             (2, Identifier {name = Data.Text.pack "a"}),
                             (2, Operator {name = Data.Text.pack "<-"}), 
                             (2, Literal {content = Data.Text.pack "0"}),
                             (3, In),
                             (3, Identifier {name = Data.Text.pack "a"}),
                             (3, Operator {name = Data.Text.pack "**"}),
                             (3, Literal {content = Data.Text.pack "20i"})
                             ]
                Parskell.Lexing.Lexer.lexing line @?= Right tokens