{-# LANGUAGE OverloadedStrings #-}

module Lexer where
import           Text.Megaparsec                ( Parsec
                                                , anySingle
                                                , unexpected
                                                , eof
                                                , anySingleBut
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Error         as E
import           Data.Void                      ( Void )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Tokens                        as T
import           Control.Applicative            ( (<|>) )
import qualified Control.Applicative.Combinators
                                               as Comb
import           Data.List.NonEmpty             ( fromList )
import           Text.Megaparsec.Pos            ( unPos )
import           Control.Monad                  ( void )

type Token = T.Token Text Int
type Lexer = Parsec Void Text


data Line = Line Int [Token] deriving Show

symbol :: Text -> Token -> Lexer Token
symbol s t = do
    L.symbol space s
    return t

space = Comb.choice $ Comb.skipMany <$> [C.char ' ', C.char '\t']

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme space

ifParser = symbol "if" T.If

elseParser = symbol "else" T.Else

whileParser = symbol "while" T.While

defParser = symbol "def" T.Def

keywordParser = Comb.choice [ifParser, elseParser, whileParser, defParser]

lParenParser = symbol "(" (T.Parenthesis T.Left)

rParenParser = symbol ")" (T.Parenthesis T.Right)

parenParser = lParenParser <|> rParenParser

plusParser = symbol "+" (T.Operator T.Plus)

minusParser = symbol "-" (T.Operator T.Minus)

multiplyParser = symbol "*" (T.Operator T.Multiply)

divideParser = symbol "/" (T.Operator T.Divide)

operatorParser =
    Comb.choice [multiplyParser, divideParser, plusParser, minusParser]

assignParser = symbol "=" T.Assign

colonParser = symbol ":" T.Colon

compareParser =
    Comb.choice [geParser, gtParser, leParser, ltParser, neqParser, eqParser]

gtParser = symbol ">" (T.Compare T.GT)

geParser = symbol ">=" (T.Compare T.GE)

leParser = symbol "<=" (T.Compare T.LE)

ltParser = symbol "<" (T.Compare T.LT)

eqParser = symbol "==" (T.Compare T.EQ)

neqParser = symbol "!=" (T.Compare T.NEQ)

identifierParser = lexeme $ do
    chars <- Comb.some idCharParser
    return (T.Identifier (pack chars))

idCharParser = C.letterChar <|> C.char '_'

stringParser = lexeme $ do
    str <- Comb.between quotesParser quotesParser innerStringParser
    return (T.String str)


quotesParser = C.char '"'

innerStringParser = do
    s <- Comb.manyTill anySingle quotesParser
    return (pack s)

numberParser = do
    num <- L.signed space (lexeme L.decimal)
    return (T.Number num)

tokenParser = Comb.choice
    [ keywordParser
    , parenParser
    , operatorParser
    , compareParser
    , assignParser
    , colonParser
    , numberParser
    , stringParser
    , identifierParser
    , unexpectedParser
    ]

unexpectedParser :: Lexer Token
unexpectedParser = do
    c <- anySingleBut '\n'
    unexpected (E.Tokens (fromList [c]))


lineParser :: Lexer Line
lineParser = do
    space
    pos    <- L.indentLevel
    tokens <- Comb.many tokenParser
    return (Line (unPos pos) tokens)


programParser :: Lexer [Line]
programParser = Comb.sepEndBy lineParser (void C.eol)
