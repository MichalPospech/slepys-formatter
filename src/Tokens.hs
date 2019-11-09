module Tokens where

data Op = Plus | Minus | Multiply | Divide deriving (Show)
data Comp = EQ | NEQ | GE | GT | LE | LT deriving (Show)
data Paren = Left | Right deriving (Show)
data Token s a = Identifier s | Operator Op | Number a | String s | Def | If | Else | While | Colon | Assign | Compare Comp | Parenthesis Paren | Indent | Dedent deriving (Show)
