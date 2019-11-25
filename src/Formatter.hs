{-# LANGUAGE NoImplicitPrelude #-}

module Formatter (programToDoc) where
import           Prelude                        ( ($)
                                                , (.)
                                                , (>)
                                                , not
                                                )
import           Data.List
import           Text.PrettyPrint               ( (<>)
                                                , (<+>)
                                                , text
                                                , int
                                                , comma
                                                , space
                                                , hcat
                                                , Doc
                                                , lparen
                                                , rparen
                                                , colon
                                                , parens
                                                , punctuate
                                                , nest
                                                , braces
                                                , empty
                                                , semi
                                                , nest
                                                , vcat
                                                )
import           Program
import           Data.Text                      ( unpack )


class PDoc a where
    pdoc :: a -> Doc

instance PDoc Comp where
    pdoc LE  = text "<="
    pdoc GE  = text ">="
    pdoc LT  = text "<"
    pdoc GT  = text ">"
    pdoc EQ  = text "=="
    pdoc NEQ = text "!="

instance PDoc Op where
    pdoc Plus     = text "+"
    pdoc Minus    = text "-"
    pdoc Divide   = text "/"
    pdoc Multiply = text "*"

instance PDoc Expression where
    pdoc (Number x) = int x
    pdoc (FuncCall id exs) =
        text (unpack id)
            <> (parens . hcat $ punctuate (comma <> space) (map pdoc exs))
    pdoc (ArithmeticExpression ex1 op ex2) = pdoc ex1 <+> pdoc op <+> pdoc ex2
    pdoc (BooleanExpression ex1 comp ex2) = pdoc ex1 <+> pdoc comp <+> pdoc ex2

instance PDoc Statement where
    pdoc (ProcCall id exs) =
        text (unpack id)
            <> (parens . hcat $ punctuate (comma <> space) (map pdoc exs))
            <> semi
    pdoc (Assignment id ex) = text (unpack id) <+> text "=" <+> pdoc ex <> semi
    pdoc (FuncDefinition id ids stats) =
        text "def"
            <+> text (unpack id)
            <>  lparen
            <>  (parens . hcat $ punctuate (comma <> space)
                                           (map (text . unpack) ids)
                )
            <>  colon
            <>  braces (nest 1 (statementsToDoc stats))
    pdoc (IfStatement ex stats1 stats2) =
        text "if"
            <+> parens (pdoc ex)
            <>  colon
            <>  braces (nest 1 (statementsToDoc stats1))
        <>  (if not $ null stats2
                    then text "else" <> colon <> braces
                        (nest 1 (statementsToDoc stats2))
                    else empty
                )
    pdoc (While ex stats) = text "while"
        <+> parens (pdoc ex)
        <>  colon
        <>  braces (nest 1 (statementsToDoc stats))
    
statementsToDoc stats = vcat $ map pdoc stats

programToDoc :: [Statement] -> Doc
programToDoc p = vcat $ map pdoc p