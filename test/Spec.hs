import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import ParskellTest.Parsing.ParserTest
import ParskellTest.Lexing.LexerTest
import ParskellTest.Synthesis.CounterTest
import ParskellTest.Synthesis.LetifierTest


main :: IO ()
main = hspec $ do
           describe "ParskellTest.ExpressionTree" $ do
               describe "ParserTest" ParskellTest.Parsing.ParserTest.parskellTest
               describe "LexerTest" ParskellTest.Lexing.LexerTest.lexerTest
               describe "CounterTest" ParskellTest.Synthesis.CounterTest.counterTest
               describe "LetifierTest" ParskellTest.Synthesis.LetifierTest.letifierTest
