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
instance Eq Token where
    (==) RoundBracketOpen RoundBracketOpen = True
    (==) RoundBracketClose RoundBracketClose = True
    (==) SquareBracketOpen SquareBracketOpen = True
    (==) SquareBracketClose SquareBracketClose = True
    (==) Identifier {name = n1} Identifier {name = n2} = n1 == n2
    (==) LiteralNumber {content = c1} LiteralNumber {content = c2} = c1 == c2
    (==) LiteralString {content = c1} LiteralString {content = c2} = c1 == c2
    (==) Let Let = True
    (==) In In = True
    (==) Do Do = True
    (==) Return Return = True
    (==) Done Done = True
    (==) Print Print = True
    (==) Operator {name = n1} Operator {name = n2} = n1 == n2
    (==) Semicolon Semicolon = True
    (==) Newline Newline = True
    (==) Ignore Ignore = True
    (==) _ _ = False
instance Show Token where
    show RoundBracketOpen = "("
    show RoundBracketClose = ")"
    show SquareBracketOpen = "["
    show SquareBracketClose = "]"
    show Identifier {name = n} = " " ++ Data.Text.unpack n ++ " <i> "
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
