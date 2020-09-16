import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import ParskellTest.ExpressionTree.ParserTest
import ParskellTest.Lexing.LexerTest


main :: IO ()
main = hspec $ do
           describe "ParskellTest.ExpressionTree" $ do
               describe "ParserTest" ParskellTest.ExpressionTree.ParserTest.parskellTest
               describe "LexerTest" ParskellTest.Lexing.LexerTest.lexerTest