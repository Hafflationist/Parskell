module Parskell.Lexing.Tokens (Token(..)) where

import Data.Text

data Token
    = RoundBracketOpen
    | RoundBracketClose
    | SquareBracketOpen
    | SquareBracketClose
    | Identifier {name :: Text}
    | Literal {content :: Text}
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
    (==) Literal {content = c1} Literal {content = c2} = c1 == c2
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
    show Literal {content = c} = " " ++ Data.Text.unpack c ++ " <l> "
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
