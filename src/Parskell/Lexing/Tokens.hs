module Parskell.Lexing.Tokens (Token(..)) where

import Data.Text

data Token
    = RoundBracketOpen
    | RoundBracketClose
    | SquareBracketOpen
    | SquareBracketClose
    | Identifier {name :: Text}
    | LiteralNumber {content :: Text}
    | LiteralString {content :: Text}
    | Let
    | In
    | Return
    | Do
    | Done
    | Print
    | Operator {name :: Text}
    | Semicolon
    | Newline
    | Ignore
    deriving Eq
instance Show Token where
    show RoundBracketOpen = "("
    show RoundBracketClose = ")"
    show SquareBracketOpen = "["
    show SquareBracketClose = "]"
    show Identifier {name = n} = " " ++ Data.Text.unpack n ++ " <ident> "
    show LiteralNumber {content = c} = " " ++ Data.Text.unpack c ++ " <lnum> "
    show LiteralString {content = c} = " " ++ Data.Text.unpack c ++ " <lstr> "
    show Let = " let "
    show In = " in "
    show Return = " return "
    show Do = " do "
    show Done = " done "
    show Print = " print "
    show Operator {name = n} = " " ++ Data.Text.unpack n ++ " <o> "
    show Semicolon = "; "
    show Newline = "<\\n> "
    show Ignore = "<ignore>"
