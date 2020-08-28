module Parskell.ExpressionTree.Conversion where

import Data.Text
import Parskell.ExpressionTree



binaryOperator2Text :: BinaryOperator -> Text
binaryOperator2Text Addition = Data.Text.pack "+"
binaryOperator2Text Subtraction = Data.Text.pack "-"
binaryOperator2Text Multiplication = Data.Text.pack "*"
binaryOperator2Text Division = Data.Text.pack "/"
